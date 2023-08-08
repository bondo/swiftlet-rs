use itertools::intersperse;
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
                        quote!(#name: #name.unwrap())
                    });
                    let parse_fields = intersperse(fields_named.named.iter().map(generate_named_struct_field), quote!(
                        if !cursor.goto_next_sibling() {
                            errors.push(::parser_common::not_implemented_error(node, "next sibling", #str_ident));
                            return Err(errors);
                        }
                    ));

                    quote!(
                        impl #impl_generics ::parser_common::Extract for #ident #ty_generics #where_clause {
                            fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<::parser_common::ParseError>> {
                                debug_assert_eq!(node.kind(), #kind);

                                let mut cursor = node.walk();

                                if !cursor.goto_first_child() {
                                    return Err(vec![::parser_common::not_implemented_error(node, "first child", #str_ident)]);
                                }

                                let mut errors: Vec<::parser_common::ParseError> = vec![];

                                #(#parse_fields)*

                                if cursor.goto_next_sibling() {
                                    errors.push(::parser_common::not_implemented_error(cursor.node(), "no more siblings", #str_ident));
                                }

                                if errors.len() > 0 {
                                    Err(errors)
                                } else {
                                    Ok(#ident{#(#field_values),*})
                                }
                            }


                            fn can_extract_kind(kind: &str) -> bool {
                                kind == #kind
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
                    let value = generate_struct_value(field);

                    quote!(
                        impl #impl_generics ::parser_common::Extract for #ident #ty_generics #where_clause {
                            fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<::parser_common::ParseError>> {
                                debug_assert_eq!(node.kind(), #kind);

                                let mut cursor = node.walk();

                                if !cursor.goto_first_child() {
                                    return Err(vec![::parser_common::not_implemented_error(node, "first child", #str_ident)]);
                                }

                                let mut errors: Vec<::parser_common::ParseError> = vec![];

                                let value: Option<#ty> = #value;

                                if cursor.goto_next_sibling() {
                                    errors.push(::parser_common::not_implemented_error(cursor.node(), "no more siblings", #str_ident));
                                }

                                if errors.len() > 0 {
                                    Err(errors)
                                } else {
                                    Ok(#ident(value.unwrap()))
                                }
                            }


                            fn can_extract_kind(kind: &str) -> bool {
                                kind == #kind
                            }
                        }
                    )
                }
                Fields::Unit => unimplemented!("unit data struct"),
            }
        }
        Data::Enum(data_enum) => {
            let cases = data_enum.variants.iter().map(generate_enum_variant);
            let kind_cases = data_enum.variants.iter().map(|variant| {
                let kind = extract_select(&variant.attrs);
                quote! (#kind => true)
            });

            quote! {
                impl #impl_generics ::parser_common::Extract for #ident #ty_generics #where_clause {
                    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<::parser_common::ParseError>> {
                        if node.is_missing() {
                            return Err(vec![::parser_common::missing_error(node)]);
                        }
                        if node.is_error() {
                            return Err(vec![::parser_common::parse_error(node)]);
                        }
                        match node.kind() {
                            #(#cases),*,
                            _ => Err(vec![::parser_common::not_implemented_error(node, "enum constructor", #str_ident)]),
                        }
                    }


                    fn can_extract_kind(kind: &str) -> bool {
                        match kind {
                            #(#kind_cases),*,
                            _ => false,
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

fn generate_named_struct_field(field: &Field) -> proc_macro2::TokenStream {
    let ident = field.ident.as_ref().expect("field should have name");
    let ty = &field.ty;
    let value = generate_struct_value(field);

    quote!(
        let #ident: Option<#ty> = #value;
    )
}

fn generate_struct_value(field: &Field) -> proc_macro2::TokenStream {
    let ty = &field.ty;

    quote!(
        if cursor.node().is_missing() {
            errors.push(::parser_common::missing_error(cursor.node()));
            None
        } else if cursor.node().is_error() {
            errors.push(::parser_common::parse_error(cursor.node()));
            None
        } else {
            match <#ty as ::parser_common::Extract>::extract(cursor.node(), source) {
                Ok(v) => Some(v),
                Err(mut errs) => {
                    errors.append(&mut errs);
                    None
                }
            }
        }
    )
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
                #select => Ok(Self::#variant_ident(<#ty as ::parser_common::Extract>::extract(node, source)?))
            )
        }
    }
}
