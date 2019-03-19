## warn
this crate is under development, it will not works! And everything may change! 

## goal
```rust

use cmd_rs::{ command, option, init, bind };

#[commond([rm <dirs...>], "remove files")]
#[option([-f, --force], "force to remove")]
#[option([-r, --recursive], "remove recursively")]
fn rm(dirs: Vec<String>) {
    /*
        do something 
    */
}

#[init]
fn main() {
    bind![rm];
}
```
when you input `<your pkg> rm ./* --all --force`, all files will be removed!
when you input `<your pkg> rm --help`, help information will display, such as:
```
Usage:

<your pkg> rm [options]

Options:

-f, --force       force to remove
-r, --recursive   remove recursively
-h, --help        display help information
```

## usage

## principles
If you use this crate in your CLI application, you have to follow those principles:

+ do not support multi-world options such as `--apple-pen`
> this is because that Rust is static language, it does not support runtime key defined by user,
we don't want to deal with it in compilation time, it's difficult and bad.
+ only <strong>the last command</strong> will run if you input many commands
+ optional arguments can only be the last argument of a command or a option
+ options are public instead of private, it means that all commands can get them
+ using `Rust 2018` stable or nightly, my version is:
    - rustc 1.35.0-nightly (e68bf8ae1 2019-03-11)
    - cargo 1.35.0-nightly (95b45eca1 2019-03-06)
+ proc macros are necessary, check documents for more information, sometimes you need to add attributes below
    - `#![feature(proc_macro_hygiene)]`
    - `#![feature(custom_attribute)]`
    - some others
> Note: `Rust`'s compiler provides ergonomic error tips, most of the time, you can improve your program easily
through compiler's tips 
+ options only accept one argument, maybe optional or required, and only be single
> define like this: `-t, --test <arg...>`, and run it with `mycli --test 0 1 2 3`, `1 2 3` will be ignored.