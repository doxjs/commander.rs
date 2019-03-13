#![recursion_limit="128"]
#![allow(dead_code)]
#![allow(unused_variables)]

extern crate proc_macro;

use proc_macro::{TokenStream };
use proc_macro2::{ TokenStream as TokenStream2, Span as Span2 };
use syn::parse::{ Parse, ParseStream, Result, Error };
use syn::{ Ident, token, LitStr, ItemFn, FnArg };
use syn::{ parse_macro_input, Token, bracketed };
use syn::spanned::Spanned;
use syn::punctuated::Punctuated;
use quote::{ quote, quote_spanned, ToTokens };
use cmd_core::{ Command, Argument, ArgumentType, Quantity };
use std::process::exit;

const ERR_RED: &'static str = "\x1b[0;31m Compile time error \x1b[0m";

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

macro_rules! is_match {
    ($span: ident => $ty: ident of $def: expr) => {
        quote_spanned!{$span =>
            {
                $def as #$ty;
            }
        }
    };
}

#[derive(Debug, PartialEq)]
enum QuantityToken {
    Single,
    Multiple,
}

#[derive(Debug, PartialEq)]
enum ArgumentTypeToken {
    Required(QuantityToken),
    Optional(QuantityToken),
}

#[derive(Debug)]
struct ArgumentToken {
    name: Ident,
    tp: ArgumentTypeToken,
}

struct CommandToken {
    name: Ident,
    args: Vec<ArgumentToken>,
    desc: Option<LitStr>,
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

impl ToTokens for CommandToken {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let CommandToken {
            name,
            args,
            desc,
        } = self;
        let name = format!("{}", name);
        let desc = if let Some(litstr) = desc {
            litstr.value()
        } else {
            String::new()
        };
        let expand = quote! {
            _cmd_Command {
                name: String::from(#name),
                args: vec![#( #args ),*],
                desc: String::from(#desc),
            }
        };

        expand.to_tokens(tokens);
    }
}

struct CommandsInputToken {
    value: Punctuated<Ident, Token![,]>,
}

impl Parse for CommandToken {
    fn parse(tokens: ParseStream) -> Result<Self> {
        let input;
        bracketed!(input in tokens);
        let name = input.parse()?;

        let res = if input.is_empty() {
            CommandToken {
                name,
                args: vec![],
                desc: None,
            }
        } else {
            let mut args: Vec<ArgumentToken> = vec![];
            let mut desc: Option<LitStr> = None;

            while !input.is_empty() {
                let lookhead = input.lookahead1();

                // required arguments should defined ahead of optional arguments
                // these two kinds of arguments can't cross appear crossed
                if lookhead.peek(token::Lt) {
                    input.parse::<token::Lt>()?;

                    let mut arg_type = ArgumentTypeToken::Required(QuantityToken::Single);
                    let arg_name: Ident = input.parse()?;
                    let span = arg_name.span();

                    if check_duplicate(&args, &arg_name) {
                        return Err(Error::new(span, format!("{}: Duplicate definition", ERR_RED)));
                    }

                    if let Ok(_) = input.parse::<token::Dot3>() {
                        arg_type = ArgumentTypeToken::Required(QuantityToken::Multiple);
                    }

                    if !args.is_empty() {
                        let tail = args.get(args.len() - 1).unwrap();

                        if let ArgumentTypeToken::Optional(_) = tail.tp {
                            return Err(input.error(format!("{}:All [Optional Arguments] should be placed after all [Required Arguments]!", ERR_RED)));
                        }
                    }
                    // advancing the next token
                    input.parse::<token::Gt>()?;

                    if let ArgumentTypeToken::Required(QuantityToken::Multiple) = arg_type {
                        if !input.is_empty() {
                            return Err(Error::new(span, format!("{}:[Multiple Optional ArgumentToken] should and only should be the last one argument!", ERR_RED)));
                        }
                    }
                    args.push(ArgumentToken {
                        name: arg_name,
                        tp: arg_type,
                    });
                } else if lookhead.peek(token::Bracket) {
                    let mut arg_type = ArgumentTypeToken::Optional(QuantityToken::Single);
                    let content;
                    let arg_name: Ident;

                    bracketed!(content in input);
                    arg_name = content.parse()?;

                    if check_duplicate(&args, &arg_name) {
                        return Err(content.error(format!("{}:Duplicate definition of this [ArgumentToken]", ERR_RED)));
                    }

                    if let Ok(_) = content.parse::<token::Dot3>() {
                        if input.is_empty() {
                            arg_type = ArgumentTypeToken::Optional(QuantityToken::Multiple);
                        } else {
                            return Err(content.error(format!("{}:[Multiple Optional ArgumentToken] should and only should be the last one argument!", ERR_RED)));
                        }
                    }

                    args.push(ArgumentToken {
                        name: arg_name,
                        tp: arg_type,
                    });
                } else {
                    return Err(lookhead.error());
                };
            };

            if tokens.peek(token::Comma) && tokens.peek2(LitStr) {
                tokens.parse::<token::Comma>()?;
                desc = tokens.parse()?;
            }

            CommandToken {
                name,
                args,
                desc,
            }
        };

        Ok(res)
    }
}

impl Parse for CommandsInputToken {
    fn parse(tokens: ParseStream) -> Result<Self> {
        Ok(CommandsInputToken {
            value: tokens.parse_terminated(Ident::parse)?,
        })
    }
}

fn check_duplicate(v: &Vec<ArgumentToken>, name: &Ident) -> bool {
    !v.iter().all(|x| &x.name != name)
}

fn build_cmd_from_token(token: &CommandToken) -> Command {
    let CommandToken {
        name,
        args,
        desc,
    } = token;
    let name = format!("{}", name);
    let desc = match desc.as_ref() {
        Some(lit_str) => lit_str.value(),
        None => String::new(),
    };
    let args: Vec<Argument> = {
        let mut args_vec = vec![];

        for arg_token in args {
            let name = format!("{}", arg_token.name);
            let arg: Argument =  match arg_token.tp {
                ArgumentTypeToken::Optional(QuantityToken::Single) => {
                    Argument {
                        name,
                        tp: ArgumentType::Optional(Quantity::Single),
                    }
                },
                ArgumentTypeToken::Optional(QuantityToken::Multiple) => {
                    Argument {
                        name,
                        tp: ArgumentType::Optional(Quantity::Multiple),
                    }
                },
                ArgumentTypeToken::Required(QuantityToken::Single) => {
                    Argument {
                        name,
                        tp: ArgumentType::Required(Quantity::Single),
                    }
                },
                ArgumentTypeToken::Required(QuantityToken::Multiple) => {
                    Argument {
                        name,
                        tp: ArgumentType::Required(Quantity::Multiple),
                    }
                },
            };

            args_vec.push(arg);
        }

        args_vec
    };

    Command::new(name, args, desc)
}

fn gen_check_arguments(inputs: &Punctuated<FnArg, token::Comma>, cmd: &Command) -> Vec<TokenStream2> {
    let mut tokens: Vec<TokenStream2> = vec![];
    let args = &cmd.args;

    if inputs.len() != args.len() {
        eprintln!("the length of arguments is [{}], which is not equal to [{}]", inputs.len(), args.len());
        exit(0);
    }

    for (idx, arg) in inputs.iter().enumerate() {
        if let FnArg::Captured(captured) = arg {
            let ty = &captured.ty;
            let span = captured.span();
            let is_match_token = match args[idx].tp {
                ArgumentType::Required(Quantity::Single) => {
                    is_match!(span => ty of String::new())
                },
                ArgumentType::Optional(Quantity::Single) => {
                    is_match!(span => ty of Some(String::new()))
                },
                ArgumentType::Required(Quantity::Multiple) => {
                    is_match!(span => ty of vec![String::new()])
                },
                ArgumentType::Optional(Quantity::Multiple) => {
                    is_match!(span => ty of Some(vec![String::new()]))
                }
            };

            tokens.push(is_match_token);
        } else {
            eprintln!("invalid argument");
            exit(0);
        }
    }

    tokens
}

fn gen_fn_inputs(cmd: &Command, n: &Ident) -> Vec<TokenStream2> {
    let args = &cmd.args;
    let mut v = vec![];

    for arg in args {
        match arg.tp {
            ArgumentType::Required(Quantity::Single) => {
                v.push(quote! {
                    {
                        if let Some(mut val) = #n.pop() {
                            if val.len() > 0 {
                                val.remove(0)
                            } else {
                                String::new()
                            }
                        } else {
                            String::new()
                        }
                    }
                })
            },
            ArgumentType::Required(Quantity::Multiple) => {
                v.push(quote! {
                   {
                        let res = if let Some(val) = #n.pop() {
                            val
                        } else {
                            vec![]
                        }
                   }
                });
            },
            ArgumentType::Optional(Quantity::Single) => {
                v.push(quote! {
                    {
                        if let Some(mut val) = #n.pop() {
                            if val.len() > 0 {
                                Some(val.remove(0))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                });
            },
            ArgumentType::Optional(Quantity::Multiple) => {
                v.push(quote! {
                   {
                        if let Some(val) = #n.pop() {
                            if val.len() > 0 {
                                Some(val)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                   }
                });
            }
        }
    }

    v
}

#[proc_macro_attribute]
pub fn command(args: TokenStream, input: TokenStream) -> TokenStream {
    let cmd_token: CommandToken = parse_macro_input!(args as CommandToken);
    let action: ItemFn = parse_macro_input!(input as ItemFn);
    let CommandToken {
        name,
        ..
    } = &cmd_token;
    let inputs = &action.decl.inputs;
    let cmd = build_cmd_from_token(&cmd_token);

    if format!("{}", action.ident) != format!("{}", name) {
        eprintln!("{}: the name of function [{}] is not equal to [{}]", ERR_RED, action.ident, name);
        exit(0);
    }

    let match_tokens  = gen_check_arguments(&inputs, &cmd);
    let post_process = Ident::new(&format!("_cmd_post_{}", name), name.span());
    let pre_process = Ident::new(&format!("_cmd_pre_{}", name), name.span());
    let name_str = format!("{}", &name);
    let name_identifier = Ident::new("_cmd_temp_ident", Span2::call_site());
    let inputs = gen_fn_inputs(&cmd, &name_identifier);

    TokenStream::from(quote! {
        #action

        fn #pre_process() {
            #( #match_tokens )*
            unsafe {
                (*COMMON_COMMANDS).lock().unwrap().push(#cmd_token);
                (*COMMON_FNS_MAP).lock().unwrap().insert(String::from(#name_str), #post_process);
            }
        }

        fn #post_process(mut #name_identifier: _cmd_InsSlice) {
            #name_identifier.reverse();
            #name(#(#inputs),*);
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
    }

    TokenStream::from(quote! {
        {
            #(
                (*COMMON_PRE_FNS).lock().unwrap().push(#pre_fns);
            )*
        }
    })
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