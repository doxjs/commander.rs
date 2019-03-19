#![recursion_limit="128"]

extern crate proc_macro;

use proc_macro::{TokenStream };
use syn::{ ItemFn, Ident, Token };
use syn::parse::{ Parse, ParseStream, Result };
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::punctuated::Punctuated;
use quote::{ quote, quote_spanned };
use cmd_compilation::{ import };

struct CommandsInputToken {
    value: Punctuated<Ident, Token![,]>,
}

impl Parse for CommandsInputToken {
    fn parse(tokens: ParseStream) -> Result<Self> {
        Ok(CommandsInputToken {
            value: tokens.parse_terminated(Ident::parse)?,
        })
    }
}

#[proc_macro_attribute]
pub fn init(_: TokenStream, input: TokenStream) -> TokenStream {
    let main_fn: ItemFn = parse_macro_input!(input as ItemFn);
    let span = main_fn.span();
    let imports = vec![
        import!(Argument as _cmd_Argument),
        import!(ArgumentType as _cmd_ArgumentType),
        import!(Quantity as _cmd_Quantity),
        import!(Command as _cmd_Command),
        import!(lazy_static as _cmd_lazy_static),
        import!(normalize as _cmd_normalize),
        import!(parse_cmd as _cmd_parse_cmd),
        import!(InsSlice as _cmd_InsSlice),
        import!(Instance as _cmd_Instance),
        import!(Options as _cmd_Options),
        import!(HashMap as _cmd_HashMap from std::collections),
        import!(Mutex as _cmd_Mutex from std::sync),
    ];
    let ItemFn {
        block,
        ..
    } = &main_fn;

    TokenStream::from(quote_spanned! {span=>
        #(#imports)*

        _cmd_lazy_static! {
            pub static ref COMMON_COMMANDS: _cmd_Mutex<Vec<_cmd_Command>> = _cmd_Mutex::new(vec![]);
            pub static ref COMMON_PRE_FNS: _cmd_Mutex<Vec<fn()>> = _cmd_Mutex::new(vec![]);
            pub static ref COMMON_FNS_MAP: _cmd_Mutex<_cmd_HashMap<String, fn(_: _cmd_InsSlice)>> = _cmd_Mutex::new(_cmd_HashMap::new());
            pub static ref COMMON_OPTIONS: _cmd_Mutex<_cmd_HashMap<String, Vec<_cmd_Options>>> = _cmd_Mutex::new(_cmd_HashMap::new());
        }

        fn main() {
            #block

            {
                (*COMMON_PRE_FNS).lock().unwrap().iter().for_each(|f| f());

                let ( cmd_ins, option_ins ) = _cmd_normalize(std::env::args(), &(*(*COMMON_COMMANDS).lock().unwrap()));

                if let Some(ins) = cmd_ins {
                    for cmd in &(*(*COMMON_COMMANDS).lock().unwrap()) {
                        if cmd.name == ins.name {
                            let slice = _cmd_parse_cmd(&ins, &cmd);

                            if let Some(f) = (*(*COMMON_FNS_MAP).lock().unwrap()).get(&cmd.name) {
                                f(slice);
                            }

                            break;
                        }
                    }
                }
            }

        }
    })
}

#[proc_macro]
pub fn bind(tokens: TokenStream) -> TokenStream {
    let cmds_token: CommandsInputToken = parse_macro_input!(tokens as CommandsInputToken);
    let parameters = cmds_token.value;
    let mut pre_fns = vec![];

    for fn_name in parameters.iter() {
        pre_fns.push(Ident::new(&format!("_cmd_pre_{}", fn_name), fn_name.span()));
        // pre_fns.push(Ident::new(&format!("_opt_pre_{}", fn_name), fn_name.span()));
    }

    TokenStream::from(quote! {
        {
            #(
                (*COMMON_PRE_FNS).lock().unwrap().push(#pre_fns);
            )*
        }
    })
}