#![allow(dead_code)]
#![allow(unused_variables)]

pub use lazy_static::*;

use std::env::{ Args };
use regex::{ Regex };
use std::process::exit;

const ERR_RED: &'static str = "\x1b[0;31mError\x1b[0m";

#[derive(Debug)]
pub enum Quantity {
    Single,
    Multiple,
}

#[derive(Debug)]
pub enum ArgumentType {
    Required(Quantity),
    Optional(Quantity),
}

#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub tp: ArgumentType,
}

#[derive(Debug)]
pub struct Command {
    pub name: String,
    pub args: Vec<Argument>,
    pub desc: String,
}

#[derive(Debug)]
pub struct Instance {
    pub name: String,
    pub args: Vec<String>,
}

impl Instance {
    fn new(name: String, args: Vec<String>) -> Self {
        Instance {
            name,
            args,
        }
    }

    fn empty() -> Self {
        Instance {
            name: String::new(),
            args: vec![],
        }
    }

    fn is_empty(&self) -> bool {
        self.name.len() == 0
    }

    pub fn push_to(self, store: &mut (Option<Instance>, Vec<Instance>), is_cmd: bool) {
        if !self.is_empty() {
            if !is_cmd {
                store.1.push(self);
            } else {
                store.0 = Some(self);
            }
        }
    }
}

impl Command {
    pub fn new(name: String, args: Vec<Argument>, desc: String) -> Self {
        Command {
            name,
            args,
            desc,
        }
    }

    pub fn empty() -> Command {
        Command {
            name: String::new(),
            args: vec![],
            desc: String::new(),
        }
    }

    pub fn is(&self, cmd: &str) -> bool {
        &self.name == cmd
    }
}

pub fn normalize(args: Args, cmds: &Vec<Command>) -> (Option<Instance>, Vec<Instance>) {
    let cmd_name = Regex::new(r"^\w+$").unwrap();
    let complex_long = Regex::new(r"^--(\w{2,})=(.+)$").unwrap();
    let short = Regex::new(r"^-(\w+)$",).unwrap();
    let long = Regex::new(r"^--\w{2,}$").unwrap();
    let mut store: (Option<Instance>, Vec<Instance>) = (None, vec![]);
    let mut enumerate = args.into_iter().enumerate();
    let mut temp_ins = Instance::empty();
    let mut cmd_exist = false;

    while let Some((idx, arg)) = enumerate.next() {
        if short.is_match(&arg) {
            if let Some(caps) = short.captures(&arg) {
                let mut temp_args: Vec<&str> = (&caps[1]).split("").collect();

                temp_args.dedup();
                for x in temp_args {
                    if x.len() > 0 {
                        temp_ins.push_to(&mut store, cmd_exist);
                        cmd_exist = false;
                        temp_ins = Instance::new(format!("-{}", x), vec![]);
                    }
                }
            }
        } else if complex_long.is_match(&arg) {
            if let Some(caps) = complex_long.captures(&arg) {
                let mut temp_args: Vec<&str> = (&caps[2]).split_terminator(" ").collect();

                temp_args.dedup();
                temp_ins.push_to(&mut store, cmd_exist);
                cmd_exist = false;

                store.1.push(Instance::new(
                    format!("--{}", &caps[1]),
                    temp_args.iter().map(|&x| String::from(x)).collect::<Vec<String>>(),
                ));
                temp_ins = Instance::empty();
            }
        } else if long.is_match(&arg) {
            temp_ins.push_to(&mut store, cmd_exist);
            cmd_exist = false;

            temp_ins = Instance::new(arg, vec![]);
        } else if cmd_name.is_match(&arg) {
            if cmds.iter().any(|x| x.is(&arg)) {
                if !temp_ins.is_empty() {
                    temp_ins.push_to(&mut store, cmd_exist);
                }

                cmd_exist = true;
                temp_ins = Instance {
                    name: String::from(arg),
                    args: vec![],
                };
            } else {
                if !temp_ins.is_empty() {
                    temp_ins.args.push(arg);
                }
            }
        } else {
            if !temp_ins.is_empty() {
                temp_ins.args.push(arg);
            }
        }
    }

    temp_ins.push_to(&mut store, cmd_exist);
    store
}

pub type InsSlice = Vec<Vec<String>>;

pub fn parse_cmd(ins: &Instance, cmd: &Command) -> InsSlice {
    let Command {
        name,
        args,
        desc,
    } = cmd;
    let mut idxs  =vec![];

    if name != &cmd.name {
        return idxs;
    }

    let mut iter = ins.args.iter();
    for arg in args {
        let mut temp_vec: Vec<String> = vec![];
        match arg.tp {
            ArgumentType::Required(Quantity::Single) => {
                if let Some(val) = iter.next() {
                    temp_vec.push(val.clone());
                    idxs.push(temp_vec);
                } else {
                    eprintln!("{}: Command parameters do not match, using `{} {} --help` for more details",
                              ERR_RED, env!("CARGO_PKG_NAME"), cmd.name);
                    exit(0);
                }
            },
            ArgumentType::Required(Quantity::Multiple) => {
                loop {
                    if let Some(val) = iter.next() {
                        temp_vec.push(val.clone());
                    } else if temp_vec.is_empty() {
                        eprintln!("{}: Command parameters do not match, using `{} {} --help` for more details",
                                  ERR_RED, env!("CARGO_PKG_NAME"), cmd.name);
                        exit(0);
                    } else {
                        break;
                    }
                }
                idxs.push(temp_vec);
            },
            ArgumentType::Optional(Quantity::Single) => {
                if let Some(val) = iter.next() {
                    temp_vec.push(val.clone());
                }
                idxs.push(temp_vec);
            },
            ArgumentType::Optional(Quantity::Multiple) => {
                while let Some(val) = iter.next() {
                    temp_vec.push(val.clone());
                }
                idxs.push(temp_vec);
            }
        }
    }

    idxs
}

