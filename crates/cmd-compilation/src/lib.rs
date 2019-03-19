use proc_macro2::{ TokenStream as TokenStream2, Ident };
use quote::{ quote, ToTokens };
use lazy_static::lazy_static;
use std::sync::Mutex;
use std::collections::HashMap;
use cmd_core::{ Options };

#[derive(Debug, PartialEq)]
pub enum QuantityToken {
    Single,
    Multiple,
}

#[derive(Debug, PartialEq)]
pub enum ArgumentTypeToken {
    Required(QuantityToken),
    Optional(QuantityToken),
}

#[derive(Debug)]
pub struct ArgumentToken {
    pub name: Ident,
    pub tp: ArgumentTypeToken,
}

impl ToTokens for ArgumentToken {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ArgumentToken {
            name,
            tp,
        } = self;
        let mut tp_token: TokenStream2;
        let name = format!("{}", name);

        match tp {
            ArgumentTypeToken::Required(QuantityToken::Single) => {
                tp_token = fetch!(name => Required, Single);
            },
            ArgumentTypeToken::Required(QuantityToken::Multiple) => {
                tp_token = fetch!(name => Required, Multiple);
            },
            ArgumentTypeToken::Optional(QuantityToken::Single) => {
                tp_token = fetch!(name => Optional, Single);
            },
            ArgumentTypeToken::Optional(QuantityToken::Multiple) => {
                tp_token = fetch!(name => Optional, Multiple);
            },
        }
        tp_token.to_tokens(tokens);
    }
}

#[macro_export]
macro_rules! import {
    ($o: ident as $r: ident) => {
        quote! {
            use cmd_rs::{ $o as $r };
        }
    };
    ($o: ident as $r: ident from $f: path) => {
        quote! {
            use $f::{ $o as $r };
        }
    }
}

#[macro_export]
macro_rules! fetch {
    ($name: ident => $which: ident, $ty: ident) => {
        {
            quote! {
                _cmd_Argument {
                    name: format!("{}", #$name),
                    tp: _cmd_ArgumentType::$which(_cmd_Quantity::$ty),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! is_match {
    ($span: ident => $ty: ident of $def: expr) => {
        quote_spanned!{$span =>
            {
                $def as #$ty;
            }
        }
    };
}