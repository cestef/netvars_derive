use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(HasNetvar)]
pub fn hasnetvar_derive(tokens: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tokens as syn::DeriveInput);
    let mut output = TokenStream::default();
    if let syn::Data::Struct(struct_data) = input.data {
        let syn::DeriveInput {
            ident, generics, ..
        } = input;

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        output = quote! {
            impl #impl_generics HasNetvar for #ident #ty_generics #where_clause {
                unsafe fn get_field<T>(&self, offset: usize) -> T { ((self as *const Self as *const i8).add(offset) as *const T).read() }
            }
        }
        .into();
    } else {
        //Handle Error: Can use derive macro only on structs.
    }

    output
}

#[derive(Debug)]
struct NetvarData {
    pub table: String,
    pub netvar: String,
}

impl syn::parse::Parse for NetvarData {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        use std::ops::Index;
        use syn::spanned::Spanned;

        let expr: syn::Expr = input.parse()?;

        if let syn::Expr::Tuple(expr_tuple) = expr {
            if expr_tuple.elems.len() != 2 {
                return Err(syn::Error::new(
                    expr_tuple.span(),
                    "Expected tuple to have only 2 elements",
                ));
            }

            let table = if let syn::Expr::Lit(literal_element) = expr_tuple.elems.index(0) {
                if let syn::Lit::Str(str_literal) = &literal_element.lit {
                    Ok(str_literal.value())
                } else {
                    Err(syn::Error::new(
                        literal_element.span(),
                        "Invalid string literal!",
                    ))
                }
            } else {
                Err(syn::Error::new(
                    expr_tuple.span(),
                    "Expected tuple elements to be a literal",
                ))
            }?;

            let netvar = if let syn::Expr::Lit(literal_element) = expr_tuple.elems.index(1) {
                if let syn::Lit::Str(str_literal) = &literal_element.lit {
                    Ok(str_literal.value())
                } else {
                    Err(syn::Error::new(
                        literal_element.span(),
                        "Invalid string literal!",
                    ))
                }
            } else {
                Err(syn::Error::new(
                    expr_tuple.span(),
                    "Expected tuple elements to be a literal",
                ))
            }?;

            Ok(NetvarData { table, netvar })
        } else {
            Err(syn::Error::new(
                expr.span(),
                "Expected tuple are attribute argument",
            ))
        }
    }
}

#[proc_macro_attribute]
pub fn netvar(attr: TokenStream, item: TokenStream) -> TokenStream {
    use syn::parse::{Parse, ParseStream};

    let NetvarData { table, netvar } = syn::parse_macro_input!(attr as NetvarData);

    let fn_ast: syn::ItemFn = syn::parse(item.clone()).unwrap();

    let vis = &fn_ast.vis;
    let attrs = &fn_ast.attrs;

    let sig = fn_ast.sig.clone();

    let ret_ty = match fn_ast.sig.output {
        syn::ReturnType::Type(_, ty) => *ty,
        syn::ReturnType::Default => syn::parse(syn::export::From::from(quote! { () })).unwrap(),
    };

    let output = quote! {
        #(#attrs)*
        #vis #sig {
            unsafe { self.get_field::<#ret_ty>(self.__internal_get_offset(#table, #netvar)) }
        }
    };
    output.into()
}

#[proc_macro_attribute]
pub fn offset_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    use syn::parse::{Parse, ParseStream};

    let path = syn::parse_macro_input!(attr as syn::ExprPath);

    let impl_block = syn::parse_macro_input!(item as syn::ItemImpl);

    let attrs = &impl_block.attrs;
    let trait_items = &impl_block.items;
    let name = *impl_block.self_ty;
    let unsafety = impl_block.unsafety;

    let (trait_path, trait_for) = match &impl_block.trait_ {
        Some((_, _path, _for)) => (Some(_path), Some(_for)),
        None => (None, None),
    };

    let (impl_generics, ty_generics, where_clause) = &impl_block.generics.split_for_impl();

    let output = quote! {
        #(#attrs)*
        #unsafety impl #impl_generics #trait_path #trait_for #name #ty_generics #where_clause {
            unsafe fn __internal_get_offset(&self, table: &str, netvar: &str) -> usize { #path(table, netvar) }
            #(#trait_items)*
        }
    };
    output.into()
}
