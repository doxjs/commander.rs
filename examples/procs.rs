#![allow(unused_variables)]
#![feature(proc_macro_hygiene)]
#![feature(custom_attribute)]

use cmd_rs::{ init, command, bind };

#[command([rmdir <dir> [otherDirs...]], "yes")]
fn rmdir(dir: String, other_dirs: Option<Vec<String>>) {
    println!("dir is {}, other_dirs is {:#?}", dir, other_dirs);
}

#[command([cpu <dir> [other_dirs...]], "yes")]
fn cpu(dir: String, other_dirs: Option<Vec<String>>) {
}

#[init]
fn main() {
    bind![rmdir, cpu];
}
