use regex::{Regex, Captures};

use asm::{mod, LabelOrInstruction, Label, Inst, Raw};
use std::collections::HashMap;

#[deriving(Show, Eq, PartialEq)]
pub enum AST {
    Atom(String),
    Integer(i32),
    Sequence(Vec<AST>)
}

pub enum TopLevelValue {
    Const(i32),
    Fun(String)
}

pub fn parse(code: &str) -> AST {
    static REPLACE: Regex = regex!("[()]|;.*");
    let code = REPLACE.replace_all(code, |captures: &Captures| {
        match captures.at(0) {
            p @ "(" | p @ ")" => format!(" {} ", p),
            _comment => "".to_string()
        }
    });

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
    let mut globals = HashMap::new();
    let mut branch_count = 0;

    let mut ret = vec![];
    for func in fns.iter() {
        ret.push_all_move(compile_top_level(func, &mut branch_count, &mut globals))
    }

    ret
}

pub fn compile_top_level<'a>(code: &'a AST, branch_count: &mut uint,
                             globals: &mut HashMap<&'a str, TopLevelValue>) -> Vec<LabelOrInstruction> {
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

            let mut branches = vec![];
            ret.push_all_move(compile_expr(result, &arg_map, branch_count, &mut branches, globals));
            ret.push(Inst(Raw(asm::RTN)));

            ret.extend(branches.move_iter().flat_map(|branch| branch.move_iter()));
        }
        [Atom(ref cnst), Atom(ref name), Integer(x)] if cnst.as_slice() == "const" => {
            assert!(globals.insert(name.as_slice(), Const(x)),
                    "can't override global {} with value {}", name, x)
        }
        _ => fail!("invalid function declaration: {}", parts)
    }

    ret
}


pub fn compile_expr<'a>(code: &'a AST, args: &HashMap<&'a str, u32>,
                        branch_count: &mut uint,
                        branches: &mut Vec<Vec<LabelOrInstruction>>,
                        globals: &HashMap<&'a str, TopLevelValue>) -> Vec<LabelOrInstruction> {
    match *code {
        Integer(x) => vec![Inst(Raw(asm::LDC(x)))],
        Atom(ref name) => {
            match args.find(&name.as_slice()) {
                Some(&num) => vec![Inst(Raw(asm::LD(0, num)))],
                None => {
                    match globals.find(&name.as_slice()) {
                        Some(&Const(num)) => vec![Inst(Raw(asm::LDC(num)))],
                        Some(&Fun(ref name)) => vec![Inst(asm::NLDF(name.to_string()))],
                        None => vec![Inst(asm::NLDF(name.to_string()))],
                    }
                }
            }
        }
        Sequence(ref things) => {
            assert!(!things.is_empty(), "compiling empty sequence");

            let mut ret = vec![];

            let head = &things[0];

            match *head {
                Atom(ref name) if name.as_slice() == "if" => {
                    // (if cond true false)
                    let cond = compile_expr(&things[1], args, branch_count, branches, globals);

                    let label_t = format!("branch-{}", branch_count);
                    *branch_count += 1;
                    let mut t = compile_expr(&things[2], args, branch_count, branches, globals);

                    let label_f = format!("branch-{}", branch_count);
                    *branch_count += 1;
                    let mut f = compile_expr(&things[3], args, branch_count, branches, globals);

                    ret.push_all_move(cond);
                    ret.push(Inst(asm::NSEL(label_t.clone(), label_f.clone())));

                    t.insert(0, Label(label_t));
                    t.push(Inst(Raw(asm::JOIN)));

                    f.insert(0, Label(label_f));
                    f.push(Inst(Raw(asm::JOIN)));

                    branches.push(t);
                    branches.push(f);

                    return ret;
                }
                _ => {}
            }

            // compile everything except the function.
            for thing in things.tail().iter() {
                ret.push_all_move(compile_expr(thing, args, branch_count, branches, globals))
            }

            let num_args = things.len() - 1;

            match *head {
                Atom(ref head) => {
                    macro_rules! x {
                        ($inst: ident, $args: expr) => {
                            Some((asm::$inst, $args))
                        }
                    }

                    let head = head.as_slice();
                    let inst_args = match head {
                        // just executes its arguments in sequence,
                        // which has already happened at this point.
                        // (do (x y z) (a b c) (d e f))
                        "do" => {
                            return ret
                        }
                        "add" | "mul" => {
                            // (add a b c d ...)

                            let is_mul = head == "mul";

                            if num_args == 0 {
                                ret.push(Inst(Raw(asm::LDC(is_mul as i32))))
                            } else {
                                let inst = if is_mul {asm::MUL} else {asm::ADD};

                                for _ in range(0, num_args - 1) {
                                    ret.push(Inst(Raw(inst)))
                                }
                            }
                            return ret
                        }
                        "sub" => x!(SUB, 2),
                        "div" => x!(DIV, 2),
                        "cons" => x!(CONS, 2),
                        "car" => x!(CAR, 1),
                        "cdr" => x!(CDR, 1),
                        "atom" => x!(ATOM, 1),
                        "brk" => x!(BRK, 0),
                        "eq" => x!(CEQ, 2),
                        "ge" => x!(CGTE, 2),
                        _ => None
                    };
                    match inst_args {
                        Some((inst, count)) => {
                            assert!(num_args == count,
                                    "{} expects exactly {} args, but found {}",
                                    head, count, num_args);

                            ret.push(Inst(Raw(inst)));
                            return ret
                        }
                        None => {/* default */}
                    }
                }
                _ => {/* default */}
            }
            // compile the function
            ret.push_all_move(compile_expr(&things[0], args, branch_count, branches, globals));
            ret.push(Inst(Raw(asm::AP(num_args as u32))));
            ret
        }
    }
}
