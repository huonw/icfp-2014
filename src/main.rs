#![feature(macro_rules, phase)]

extern crate getopts;

extern crate regex;
#[phase(plugin)] extern crate regex_macros;

use std::{os, io, result};

mod asm;
mod ghost;
mod lisp;


fn main() {
    let opts = [getopts::optflag("g", "ghost", "compile a ghost program"),
                getopts::optopt("e", "emit", "what format to print", "bin|asm|ast"),
                getopts::optflag("h", "help", "show this help message")];

    let args = os::args();

    let mut matches = getopts::getopts(args.tail(), opts).unwrap();
    if matches.opt_present("help") {
        println!("{}", getopts::usage(args[0].as_slice(), opts));
        return;
    }

    if matches.free.is_empty() {
        matches.free.push("-".to_string());
    }

    let input: io::IoResult<Vec<String>> = result::collect(
        matches.free.iter().map(|name| {
            if name.as_slice() == "-" {
                io::stdin().read_to_string()
            } else {
                io::File::open(&Path::new(name.as_slice())).and_then(|mut r| r.read_to_string())
            }
        }));

    let input = match input {
        Ok(i) => i.concat(),
        Err(e) => {
            println!("error loading files: {}", e);
            os::set_exit_status(1);
            return
        }
    };

    if matches.opt_present("ghost") {
        let (code, var, consts) = ghost::parse(input.as_slice());
        for inst in ghost::compile(code.as_slice(), &var, &consts).iter() {
            println!("{}", *inst)
        }
    } else {
        let code = lisp::parse(input.as_slice());

        match matches.opt_str("emit").unwrap_or(String::new()).as_slice() {
            "" | "bin" => {
                print!("{}",
                       asm::print(asm::compile(lisp::compile(&code).as_slice()).as_slice()));
            }
            "asm" => {
                print!("{}",
                       asm::print_labelled(lisp::compile(&code).as_slice()))

            }
            "ast" => println!("{}", code),
            _ => fail!("unrecognised `emit`")
        }
    }
}
