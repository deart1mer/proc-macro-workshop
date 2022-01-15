use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated, spanned::Spanned, token, AngleBracketedGenericArguments, Data,
    DataStruct, DeriveInput, Error, Field, Fields, FieldsNamed, GenericArgument, Ident, Lit, Meta,
    MetaList, MetaNameValue, NestedMeta, Path, PathArguments, PathSegment, Result, Type, TypePath,
};

fn get_generic_type<'a, 'b>(ty: &'a Type, ident_str: &'b str) -> Option<&'a Type> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        let seg = segments.last().unwrap();
        if seg.ident == ident_str {
            if let PathSegment {
                arguments:
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                ..
            } = seg
            {
                let g = args.first().unwrap();
                if let GenericArgument::Type(t) = g {
                    return Some(t);
                }
            }
        }
    }
    None
}

fn parse_field_attr_each(field: &Field) -> Result<Option<proc_macro2::Ident>> {
    let parsed = field
        .attrs
        .iter()
        .map(|attr| {
            if let Ok(Meta::List(MetaList {
                ref path,
                ref nested,
                ..
            })) = attr.parse_meta()
            {
                if path.segments.first().unwrap().ident.to_string() == "builder" {
                    if let Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                        path,
                        lit,
                        ..
                    }))) = nested.first()
                    {
                        let name = &path.segments.first().unwrap().ident;
                        if name == "each" {
                            if let Lit::Str(list_str) = lit {
                                return Ok(Some(proc_macro2::Ident::new(
                                    list_str.value().as_str(),
                                    attr.span(),
                                )));
                            }
                        } else {
                            return Err(Error::new_spanned(
                                name,
                                r#"expected `builder(each = "...")`"#,
                            ));
                        }
                    }
                }
            }
            Ok(None)
        })
        .collect::<Result<Vec<Option<proc_macro2::Ident>>>>()?;
    Ok(parsed.into_iter().filter_map(|i| i).last())
}

fn generate_builder(
    builder_name: &Ident,
    struct_name: &Ident,
    fields: &punctuated::Punctuated<Field, token::Comma>,
) -> Result<proc_macro2::TokenStream> {
    let attrs = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;
            if let Some(unwrap_type) = get_generic_type(ty, "Option") {
                quote! {
                    #ident: std::option::Option<#unwrap_type>
                }
            } else if let Some(_) = get_generic_type(ty, "Vec") {
                quote! {
                    #ident: #ty
                }
            } else {
                quote! {
                    #ident: std::option::Option<#ty>
                }
            }
        })
        .collect::<Vec<_>>();
    let setters = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;
            if let Some(unwrap_type) = get_generic_type(ty, "Option") {
                Ok(quote! {
                    fn #ident(&mut self, #ident: #unwrap_type) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                })
            } else if let Some(unwrap_type) = get_generic_type(ty, "Vec") {
                if let Some(alias) = parse_field_attr_each(field)? {
                    Ok(quote! {
                        fn #alias(&mut self, #alias: #unwrap_type) -> &mut Self {
                            self.#ident.push(#alias);
                            self
                        }
                    })
                } else {
                    Ok(quote! {
                        fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident.clone();
                            self
                        }
                    })
                }
            } else {
                Ok(quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                })
            }
        })
        .collect::<Result<Vec<proc_macro2::TokenStream>>>()?;
    let guards: Vec<_> = fields
        .iter()
        .filter_map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;
            if let Some(_) = get_generic_type(ty, "Option") {
                None
            } else if let Some(_) = get_generic_type(ty, "Vec") {
                None
            } else {
                Some(quote! {
                    if (self.#ident.is_none()) {
                        let err = format!("{} field missing", stringify!(#ident));
                        return std::result::Result::Err(err.into());
                    }
                })
            }
        })
        .collect();
    let st_attr_inits: Vec<_> = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;
            if let Some(_) = get_generic_type(ty, "Option") {
                quote! {
                    #ident: self.#ident.clone()
                }
            } else if let Some(_) = get_generic_type(ty, "Vec") {
                quote! {
                    #ident: self.#ident.clone()
                }
            } else {
                quote! {
                    #ident: self.#ident.clone().unwrap()
                }
            }
        })
        .collect();
    let builder = quote! {
        pub struct #builder_name {
            #(#attrs),*
        }

        impl #builder_name {
            #(#setters)*

            pub fn build(&mut self) -> std::result::Result::<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                #(#guards)*
                let st = #struct_name {
                    #(#st_attr_inits),*
                };
                std::result::Result::Ok(st)
            }
        }
    };
    Ok(builder)
}

fn generate_struct_impl(
    builder_name: &Ident,
    struct_name: &Ident,
    fields: &punctuated::Punctuated<Field, token::Comma>,
) -> proc_macro2::TokenStream {
    let inits = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;
            if let Some(_) = get_generic_type(ty, "Vec") {
                quote! {
                    #ident: std::vec::Vec::new()
                }
            } else {
                quote! {
                    #ident: std::option::Option::None
                }
            }
        })
        .collect::<Vec<_>>();
    quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#inits),*
                }
            }
        }
    }
}

fn expand(input: DeriveInput) -> Result<proc_macro2::TokenStream> {
    let struct_name = &input.ident;
    let builder_name_literal = format!("{}Builder", struct_name.to_string());
    let builder_name = Ident::new(&builder_name_literal, struct_name.span());
    let implements = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { ref named, .. }),
            ..
        }) => {
            let struct_impl = generate_struct_impl(&builder_name, &struct_name, named);
            let builder = generate_builder(&builder_name, &struct_name, named)?;
            quote! {
                #struct_impl
                #builder
            }
        }
        _ => panic!(""),
    };
    Ok(implements)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    match expand(input) {
        Ok(generated) => generated.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
