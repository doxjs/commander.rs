#![allow(unused_variables)]
#![feature(proc_macro_hygiene)]
#![feature(custom_attribute)]

use cmd_rs::{ init, command, option };

#[option([-s, --simple], "simplify sth")]
#[command([rmdir <dir> [otherDirs...]], "yes")]
#[option([-u, --undefined], "undefined behaviour")]
fn rmdir(dir: String, other_dirs: Option<Vec<String>>) {
    println!("dir is {}, other_dirs is {:#?}", dir, other_dirs);
}

#[init]
fn main() {
    // bind![rmdir];
}
