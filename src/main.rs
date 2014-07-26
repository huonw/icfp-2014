#![feature(macro_rules)]

use std::io;

mod asm;
mod lisp;

fn main() {
    let string = io::stdin().read_to_string().unwrap();
    //let x = asm::parse(string.as_slice());
    //println!("{}", asm::compile(x.as_slice()));
    let code = lisp::parse(string.as_slice());
    println!("parsed: {}", code);
    let compiled = lisp::compile(&code);
    println!("labelled: {}", asm::print_labelled(compiled.as_slice()));
    let asm = asm::compile(compiled.as_slice());
    println!("final: {}", asm::print(asm.as_slice()));
}
