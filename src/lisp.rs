use asm::{mod, LabelOrInstruction, Label, Inst, Raw, RTN, LDC, NLDF, AP};
use std::collections::HashMap;


#[deriving(Show, Eq, PartialEq)]
pub enum AST {
    Atom(String),
    Integer(u32),
    Sequence(Vec<AST>)
}

pub fn parse(code: &str) -> AST {
    let code = code.replace("(", " ( ").replace(")", " ) ");

    let mut stack = vec![];
    let mut curr = vec![];

    for word in code.as_slice().words() {
        match word {
            "(" => {
                stack.push(curr);
                curr = vec![];
            }
            ")" => {
                let mut parent = stack.pop().expect("too many closing parentheses");
                parent.push(Sequence(curr));
                curr = parent;
            }
            _ => {
                curr.push(match from_str(word) {
                    Some(n) => Integer(n),
                    None => Atom(word.to_string())
                })
            }
        }
    }

    assert!(stack.is_empty(), "too many opening parentheses: {}\n{}", stack, code);

    Sequence(curr)
}
/*
(defun main () f)
(defun f () 1)

(if condition true false)
add/mul/...
(<x> ...)
*/
pub fn compile(code: &AST) -> Vec<LabelOrInstruction> {
    let fns = match *code {
        Atom(_) | Integer(_) => fail!("bare literal at the top level"),
        Sequence(ref fns) => fns,
    };

    let mut ret = vec![];
    for func in fns.iter() {
        ret.push_all_move(compile_fn(func))
    }

    ret
}

pub fn compile_fn(code: &AST) -> Vec<LabelOrInstruction> {
    let parts = match *code {
        Atom(_) | Integer(_) => fail!("bare literal when function expected"),
        Sequence(ref parts) => parts,
    };
    let mut ret = vec![];
    // defun <name> (...) <stuff>
    match parts.as_slice() {
        [Atom(ref defun), Atom(ref name), Sequence(ref args), ref result] if defun.as_slice() == "defun" => {
            let arg_map = args.iter().enumerate().map(|(i, arg)| {
                match *arg {
                    Atom(ref s) => (s.as_slice(), i as u32),
                    _ => fail!("invalid argument name: {}", *arg),
                }
            }).collect();

            ret.push(Label(name.to_string()));

            ret.push_all_move(compile_expr(result, &arg_map));
            ret.push(Inst(Raw(RTN)));
        }
        _ => fail!("invalid function declaration: {}", parts)
    }

    ret
}


pub fn compile_expr<'a>(code: &'a AST, args: &HashMap<&'a str, u32>) -> Vec<LabelOrInstruction> {
    match *code {
        Integer(x) => vec![Inst(Raw(LDC(x)))],
        Atom(ref name) => {
            match args.find(&name.as_slice()) {
                Some(_) => fail!(),
                None => {
                    vec![Inst(NLDF(name.to_string()))]
                }
            }
        }
        Sequence(ref things) => {
            let mut ret = vec![];
            for thing in things.iter().rev() {
                ret.push_all_move(compile_expr(thing, args));
            }
            ret.push(Inst(Raw(AP(things.len() as u32 - 1))));

            ret
        }
    }
}
