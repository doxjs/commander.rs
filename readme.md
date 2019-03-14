## warn
this crate is under development

## principles

+ only <strong>the last command</strong> will run if you input many commands
+ optional arguments can only be the last argument of a command or a option
+ options are public instead of private, it means that all commands can get them
+ using `Rust 2018` stable or nightly, my version is:
    - rustc 1.35.0-nightly (e68bf8ae1 2019-03-11)
    - cargo 1.35.0-nightly (95b45eca1 2019-03-06)
+ proc macros are necessary, check documents for more information, sometimes you need to add outer attributes follow
    - `#![feature(proc_macro_hygiene)]`
    - `#![feature(custom_attribute)]`
    - some others
> Note: `Rust`'s compiler provides ergonomic error tips, most of the time, you can improve your program easily
through compiler's tips 