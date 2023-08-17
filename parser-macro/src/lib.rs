use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Attribute, Data, DeriveInput, Field, Fields, LitStr, Variant};

#[proc_macro_derive(Extract, attributes(select, kind))]
pub fn derive_macro_from_tree_sitter(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        attrs,
        ..
    } = parse_macro_input!(input as DeriveInput);
    let str_ident = ident.to_string();

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    match data {
        Data::Struct(data_struct) => {
            let kind = extract_kind(&attrs);
            match data_struct.fields {
                Fields::Named(fields_named) =>{
                    let field_values = fields_named.named.iter().map(|f|{
                        let name = f.ident.as_ref().expect("Field should have name");
                        quote!(#name)
                    });
                    let parse_fields = fields_named.named.iter().enumerate().map(|(idx, field)| {
                        let field_ident = field.ident.as_ref().expect("field should have name");
                        let ty = &field.ty;
                        let is_last = idx == fields_named.named.len() - 1;

                        quote!(
                            if cursor.node().is_missing() {
                                return Err(::parser_common::ExtractError::Advance(vec![
                                    ::parser_common::missing_error(cursor.node()),
                                ]));
                            }

                            if cursor.node().is_error() {
                                return Err(::parser_common::ExtractError::Advance(vec![
                                    ::parser_common::parse_error(cursor.node()),
                                ]));
                            }

                            let #field_ident: #ty = match <#ty as ::parser_common::Extract>::extract(cursor.node(), source) {
                                Ok(value) => {
                                    if #is_last {
                                        if cursor.goto_next_sibling() {
                                            return Err(::parser_common::ExtractError::Advance(vec![
                                                ::parser_common::not_implemented_error(cursor.node(), "no more siblings", #str_ident),
                                            ]));
                                        }
                                    } else {
                                        if !cursor.goto_next_sibling() {
                                            return Err(::parser_common::ExtractError::Advance(vec![
                                                ::parser_common::not_implemented_error(node, "next sibling", #str_ident),
                                            ]));
                                        }
                                    }
                                    value
                                },
                                Err(::parser_common::ExtractError::Advance(errs)) => return Err(::parser_common::ExtractError::Advance(errs)),
                                Err(::parser_common::ExtractError::Skip(v)) => v,
                            };
                        )
                    });

                    quote!(
                        impl #impl_generics ::parser_common::Extract for #ident #ty_generics #where_clause {
                            fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ::parser_common::ExtractError<Self>> {
                                if node.kind() != #kind {
                                    return Err(::parser_common::ExtractError::Advance(vec![
                                        ::parser_common::not_implemented_error(node, &format!("kind {}", #kind), #str_ident),
                                    ]));
                                }

                                let mut cursor = node.walk();

                                if !cursor.goto_first_child() {
                                    return Err(::parser_common::ExtractError::Advance(vec![
                                        ::parser_common::not_implemented_error(node, "first child", #str_ident),
                                    ]));
                                }

                                #(#parse_fields)*

                                Ok(#ident{#(#field_values),*})
                            }
                        }
                    )
                }
                Fields::Unnamed(fields_unnamed) => {
                    assert_eq!(
                        1,
                        fields_unnamed.unnamed.len(),
                        "Only single unnamed field supported"
                    );
                    let field = fields_unnamed.unnamed.first().unwrap();
                    let ty = &field.ty;

                    quote!(
                        impl #impl_generics ::parser_common::Extract for #ident #ty_generics #where_clause {
                            fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ::parser_common::ExtractError<Self>> {
                                if node.kind() != #kind {
                                    return Err(::parser_common::ExtractError::Advance(vec![
                                        ::parser_common::not_implemented_error(node, &format!("kind {}", #kind), #str_ident),
                                    ]));
                                }

                                let value: #ty = match <#ty as ::parser_common::Extract>::extract(node, source) {
                                    Ok(value) => value,
                                    Err(::parser_common::ExtractError::Advance(errs)) => return Err(::parser_common::ExtractError::Advance(errs)),
                                    Err(::parser_common::ExtractError::Skip(v)) => v,
                                };

                                Ok(#ident(value))
                            }
                        }
                    )
                }
                Fields::Unit => {
                    quote!(
                        impl #impl_generics ::parser_common::Extract for #ident #ty_generics #where_clause {
                            fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ::parser_common::ExtractError<Self>> {
                                if node.kind() != #kind {
                                    return Err(::parser_common::ExtractError::Advance(vec![
                                        ::parser_common::not_implemented_error(node, &format!("kind {}", #kind), #str_ident),
                                    ]));
                                }

                                let mut cursor = node.walk();

                                if cursor.goto_first_child() {
                                    return Err(::parser_common::ExtractError::Advance(vec![
                                        ::parser_common::not_implemented_error(cursor.node(), "first child", #str_ident),
                                    ]));
                                }

                                Ok(#ident)
                            }
                        }
                    )
                }
            }
        }
        Data::Enum(data_enum) => {
            let cases = data_enum.variants.iter().map(generate_enum_variant);

            quote! {
                impl #impl_generics ::parser_common::Extract for #ident #ty_generics #where_clause {
                    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ::parser_common::ExtractError<Self>> {
                        if node.is_missing() {
                            return Err(::parser_common::ExtractError::Advance(vec![
                                ::parser_common::missing_error(node),
                            ]));
                        }
                        if node.is_error() {
                            return Err(::parser_common::ExtractError::Advance(vec![
                                ::parser_common::parse_error(node),
                            ]));
                        }
                        match node.kind() {
                            #(#cases),*,
                            _ => Err(::parser_common::ExtractError::Advance(vec![
                                ::parser_common::not_implemented_error(node, "enum constructor", #str_ident),
                            ])),
                        }
                    }
                }
            }
        },
        Data::Union(_) => panic!("Union is not supported"),
    }.into()
}

fn extract_select(attrs: &[Attribute]) -> String {
    attrs
        .iter()
        .find_map(parse_select)
        .expect("select attribute required")
}

fn parse_select(attr: &Attribute) -> Option<String> {
    if attr.path().is_ident("select") {
        let lit: LitStr = attr
            .parse_args()
            .expect("select attribute must have string argument");
        Some(lit.value())
    } else {
        None
    }
}

fn extract_kind(attrs: &[Attribute]) -> String {
    attrs
        .iter()
        .find_map(parse_kind)
        .expect("kind attribute required")
}

fn parse_kind(attr: &Attribute) -> Option<String> {
    if attr.path().is_ident("kind") {
        let lit: LitStr = attr
            .parse_args()
            .expect("kind attribute must have string argument");
        Some(lit.value())
    } else {
        None
    }
}

fn generate_enum_variant(variant: &Variant) -> proc_macro2::TokenStream {
    let variant_ident = &variant.ident;
    let select = extract_select(&variant.attrs);

    match &variant.fields {
        Fields::Named(_) => unimplemented!("Named fields not supported"),
        Fields::Unit => {
            quote!(
                #select => Ok(Self::#variant_ident)
            )
        }
        Fields::Unnamed(fields_unnamed) => {
            assert_eq!(
                1,
                fields_unnamed.unnamed.len(),
                "Only single unnamed field supported"
            );
            let field = fields_unnamed.unnamed.first().unwrap();
            let ty = &field.ty;

            quote!(
                #select => match <#ty as ::parser_common::Extract>::extract(node, source) {
                    Ok(value) => Ok(Self::#variant_ident(value)),
                    Err(::parser_common::ExtractError::Advance(errs)) => return Err(::parser_common::ExtractError::Advance(errs)),
                    Err(::parser_common::ExtractError::Skip(v)) => Err(::parser_common::ExtractError::Skip(Self::#variant_ident(v))),
                }
            )
        }
    }
}
