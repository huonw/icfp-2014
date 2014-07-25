#![feature(macro_rules)]

use std::io;

mod asm;

fn main() {
    let string = io::stdin().read_to_string().unwrap();
    println!("{}", asm::parse(string.as_slice()));
}
