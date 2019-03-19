#![recursion_limit="128"]
#![allow(unused_variables)]

extern crate proc_macro;

use proc_macro::{TokenStream };
use proc_macro2::{ TokenStream as TokenStream2, Span as Span2 };
use syn::parse::{ Parse, ParseStream, Result, Error, ParseBuffer };
use syn::{ Ident, token, LitStr, ItemFn, FnArg };
use syn::{ parse_macro_input, bracketed, Token };
use syn::spanned::Spanned;
use syn::punctuated::Punctuated;
use quote::{ quote, quote_spanned, ToTokens };
use cmd_compilation::{ QuantityToken, ArgumentToken, ArgumentTypeToken, is_match };
use lazy_static::lazy_static;
use std::process::exit;
use std::sync::Mutex;
use std::collections::HashMap;
use cmd_core::{ Command, Argument, ArgumentType, Quantity, Options };

const ERR_RED: &'static str = "\x1b[0;31m Compile time error \x1b[0m";

lazy_static! {
    static ref COM_TRANS: Mutex<HashMap<String, Vec<i32>>> = Mutex::new(HashMap::new());
}

struct CommandToken {
    name: Ident,
    args: Vec<ArgumentToken>,
    desc: Option<LitStr>,
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

// options
#[derive(Debug)]
struct OptionsToken {
    short: Ident,
    long: Ident,
    args: Vec<ArgumentToken>,
    desc: Option<LitStr>,
}

impl ToTokens for OptionsToken {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let OptionsToken {
            short,
            long,
            args,
            desc,
        } = self;
        let short = format!("{}", short);
        let long = format!("{}", long);
        let desc = if let Some(litstr) = desc {
            litstr.value()
        } else {
            String::new()
        };
        let expand = quote! {
            _cmd_Options {
                short: String::from(#short),
                long: String::from(#long),
                args: vec![#( #args ),*],
                desc: String::from(#desc),
            }
        };

        expand.to_tokens(tokens);
    }
}

// #[option([-s, --simple <name>], "hello world!")]
impl Parse for OptionsToken {
    fn parse(tokens: ParseStream) -> Result<Self> {
        let input: ParseBuffer;
        let short: Ident;
        let long: Ident;
        let mut args: Vec<ArgumentToken> = vec![];
        let mut desc: Option<LitStr> = None;

        bracketed!(input in tokens);
        // skip single line
        input.parse::<Token![-]>()?;
        short = input.parse()?;
        input.parse::<Token![,]>()?;
        // skip double line
        {
            input.parse::<Token![-]>()?;
            input.parse::<Token![-]>()?;
        }
        long = input.parse()?;

        while !input.is_empty() {
            let lookhead = input.lookahead1();

            // required arguments should defined ahead of optional arguments
            // these two kinds of arguments can't cross appear crossed
            if lookhead.peek(token::Lt) {
                input.parse::<token::Lt>()?;

                let mut arg_type = ArgumentTypeToken::Required(QuantityToken::Single);
                let arg_name: Ident = input.parse()?;
                let span = arg_name.span();

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

        Ok(OptionsToken {
            short,
            long,
            args,
            desc,
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

    if !COM_TRANS.lock().unwrap().contains_key(&name_str) {
        COM_TRANS.lock().unwrap().insert(name_str.clone(), vec![]);
    }
    println!("inserted {:#?}", (*COM_TRANS).lock().unwrap().keys());
    TokenStream::from(quote! {
        #[option([-h, --help], "show usage")]
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

fn build_opt_from_token(token: &OptionsToken) -> Options {
    let OptionsToken {
        short,
        long,
        args,
        desc,
    } = token;
    let short = format!("{}", short);
    let long = format!("{}", long);
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

    Options::new(short, long, args, desc)
}

#[proc_macro_attribute]
pub fn option(args: TokenStream, input: TokenStream) -> TokenStream {
    let action: ItemFn = parse_macro_input!(input as ItemFn);
    let ItemFn {
        ident,
        ..
    } = &action;
    let opt_token: OptionsToken = parse_macro_input!(args as OptionsToken);
    let opt_name = format!("{}", opt_token.long);
    let opt_ident = opt_token.long;

    if COM_TRANS.lock().unwrap().contains_key(&opt_name) {
        if let Some(v) = COM_TRANS.lock().unwrap().get_mut(&opt_name) {
            v.push(0);
        }
    } else {
        COM_TRANS.lock().unwrap().insert(opt_name.clone(), vec![]);
    }

    TokenStream::from(quote! {
        #action
    })
}