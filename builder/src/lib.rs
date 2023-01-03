use proc_macro2::{TokenStream, TokenTree};
use proc_macro2::TokenTree::Literal;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Type, Field};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    eprintln!("{:#?}", input);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);

    // fn f() -> Result<i64, Box<dyn Error>>

    fn is_option(t: Type) -> Option<Type> {
        if let Type::Path(path) = t {
            if path.path.segments.len() == 1 && path.path.segments[0].ident == "Option" {
                if let syn::PathArguments::AngleBracketed(g) =
                    &path.clone().path.segments[0].arguments
                {
                    if let syn::GenericArgument::Type(t) = g.args[0].clone() {
                        return Some(t);
                    }
                }
            }
        }
        return None;
    }

    fn parse_arg(field: Field) -> Option<String>{
        if field.attrs.len() != 1{
            return None;
        }
        let a = field.attrs[0].tokens.clone().into_iter().next().unwrap();
        let group = match a{
            TokenTree::Group(g) => {g}
            _ => return None,
        };
        let literal = match group.stream().clone().into_iter().nth(2).unwrap(){
            TokenTree::Literal(l) => {l}
            _ => return None,
        };
        return Some(literal.to_string())
    }

    let fields = {
        match input.data {
            Data::Struct(s) => match s.fields {
                Fields::Named(f) => f.named,
                _ => panic!("only works on named"),
            },
            _ => panic!("only works on structs"),
        }
    };
    let fields_names: Vec<_> = fields
        .clone()
        .into_iter()
        .map(|f| f.ident.clone())
        .collect();
    let fields_types: Vec<_> = fields.clone().into_iter().map(|f| f.ty.clone()).collect();

    let builder_types = fields_types
        .clone()
        .into_iter()
        .map(|t| {
            if is_option(t.clone()).is_some() {
                quote! {
                    #t
                }
            } else {
                quote! {
                    Option<#t>
                }
            }
        })
        .collect::<Vec<_>>();

    let fields_names1 = fields_names.clone();
    let builder_def = quote! {
        pub struct #builder_name {
            #(#fields_names1 : #builder_types),*
        }
    };

    // #(fn #fields_names(&mut self, #fields_names))
    let builder = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#fields_names: None,)*
                }
            }
        }
    };


    let builder_function_types = fields_types
        .clone()
        .into_iter()
        .map(|t| {
            if let Some(t) = is_option(t.clone()){
                quote! {
                    #t
                }
            } else {
                quote! {
                    #t
                }
            }
        })
        .collect::<Vec<_>>();

    let built = fields.clone().into_iter().map(|f| {
        let name = f.ident.clone().unwrap();
        let t = f.ty.clone();
        if is_option(t.clone()).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or("is none")?
            }
        }
    });
    let builder_impl = quote! {
        impl #builder_name {
            #(fn #fields_names(&mut self, #fields_names: #builder_function_types) -> &mut Self {
                self.#fields_names = Some(#fields_names);
                self
            }
            )*

            fn build(&mut self) -> Result<#name, Box<dyn Error>>{
                Ok(#name {
                    #(#built),*
                })
            }
        }
    };

    let f = quote! {
        use std::error::Error;
        #builder

        #builder_def

        #builder_impl
    };

    f.into()
}
