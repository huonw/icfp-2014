#![feature(macro_rules)]

use std::io;

mod asm;
mod lisp;

fn main() {
    let string = io::stdin().read_to_string().unwrap();
    /*let x = asm::parse(string.as_slice());
    println!("{}", asm::compile(x.as_slice()));*/
    println!("Read code as {}", lisp::parse(string.as_slice()));
}
