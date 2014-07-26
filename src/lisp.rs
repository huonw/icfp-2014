use std::io::File;
use std::rc::Rc;
use regex::{Regex, Captures};

use asm::{mod, LabelOrInstruction, Instruction, NamedInstruction, Label, Inst, Raw};
use std::collections::HashMap;

#[deriving(Show, Eq, PartialEq)]
enum AST {
    Atom(String),
    Integer(i32),
    Sequence(Vec<AST>)
}

enum TopLevelValue {
    Const(i32),
    Fun(String)
}

struct FnInfo<'a> {
    name: String,
    args: Rc<Env<'a>>,
    body: &'a [AST]
}

struct State<'a,'b> {
    // local stuff:
    fn_name: String,
    branches: Vec<Vec<LabelOrInstruction>>,
    asm: Vec<LabelOrInstruction>,
    env: Rc<Env<'a>>,
    // global stuff:
    branch_count: &'b mut uint,
    globals: &'b HashMap<&'a str, TopLevelValue>
}

#[deriving(Show)]
struct Env<'a> {
    here: HashMap<&'a str, u32>,
    parent: Option<Rc<Env<'a>>>
}

impl<'a> Env<'a> {
    fn find(&self, name: &'a str) -> Option<(u32, u32)> {
        let mut this = self;
        let mut level = 0;
        loop {
            match this.here.find(&name) {
                Some(&num) => return Some((level, num)),
                None => { }
            }
            match this.parent {
                Some(ref parent) => { this = &**parent }
                None => return None
            }
            level += 1;
        }
    }
}

pub fn parse(code: &str) -> AST {
    static INCLUDES: Regex = regex!(r#"\(\s*include\s*"([^"]*)"\s*\)"#); // " eurgh, syntax highlighter hack

    let code = INCLUDES.replace_all(code, |captures: &Captures| {
        let filename = captures.at(1);
        // errors, what errors?
        File::open(&Path::new(filename)).unwrap().read_to_string().unwrap()
    });

    static REPLACE: Regex = regex!("[()]|;.*");
    let code = REPLACE.replace_all(code.as_slice(), |captures: &Captures| {
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

fn parse_fn_decl<'a>(name: String, args: &'a AST, body: &'a [AST],
                     parent: Option<Rc<Env<'a>>>) -> FnInfo<'a> {
    let args = match *args {
        Sequence(ref args) => args,
        _ => fail!("function declaration `{}` requires arguments as a sequence")
    };

    let arg_map = args.iter().enumerate().map(|(i, arg)| {
        match *arg {
            Atom(ref s) => (s.as_slice(), i as u32),
            _ => fail!("invalid argument name: {}", *arg),
        }
    }).collect();

    FnInfo {
        name: name,
        args: Rc::new(Env { here: arg_map, parent: parent }),
        body: body
    }
}

fn top_level_pass<'a>(code: &'a [AST]) -> (HashMap<&'a str, TopLevelValue>, Vec<FnInfo<'a>>) {
    let mut globals = HashMap::new();
    let mut fns = vec![];

    for decl in code.iter() {
        // defun <name> (...) <stuff>
        let decl = match *decl {
            Atom(_) | Integer(_) => fail!("bare literal at the top level"),
            Sequence(ref decl) => decl,
        };

        match decl.as_slice() {
            [Atom(ref defun), Atom(ref name),
             ref args,
             .. body] if defun.as_slice() == "defun" => {
                assert!(globals.insert(name.as_slice(), Fun(name.to_string())),
                        "can't override function named {}", name);
                fns.push(parse_fn_decl(name.to_string(), args, body, None));
            }
            [Atom(ref cnst), Atom(ref name), Integer(x)] if cnst.as_slice() == "const" => {
                assert!(globals.insert(name.as_slice(), Const(x)),
                        "can't override global {} with value {}", name, x)
            }
            _ => fail!("invalid top level declaration: {}", decl)
        }
    }

    (globals, fns)
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
        Atom(_) | Integer(_) => fail!("bare literal supplied"),
        Sequence(ref fns) => fns,
    };
    let (globals, mut fns) = top_level_pass(fns.as_slice());

    // move `main` to be the first.
    match fns.iter().position(|f| f.name.as_slice() == "main") {
        Some(i) => {
            if i != 0 {
                let main = fns.remove(i).unwrap();
                fns.insert(0, main);
            }
        }
        None => {
            fail!("missing `main` function")
        }
    }

    let mut branch_count = 0;

    let mut asm = vec![];
    for func in fns.iter() {
        asm.push_all_move(State::compile_fn(func, &mut branch_count, &globals))
    }

    asm
}

impl<'a, 'b> State<'a, 'b> {
    pub fn compile_fn<'a>(code: &FnInfo<'a>, branch_count: &mut uint,
                          globals: &HashMap<&'a str, TopLevelValue>) -> Vec<LabelOrInstruction> {
        let (mut asm, branches) =
            State::compile_section(code.body,
                                   code.name.to_string(),
                                   code.name.to_string(),
                                   code.args.clone(), branch_count, globals, asm::RTN);
        asm.extend(branches.move_iter().flat_map(|branch| branch.move_iter()));
        asm
    }

    fn compile_section(body: &'a [AST], name: String, fn_name: String,
                       env: Rc<Env<'a>>, branch_count: &mut uint,
                       globals: &HashMap<&'a str, TopLevelValue>, tail: Instruction)
                           -> (Vec<LabelOrInstruction>, Vec<Vec<LabelOrInstruction>>) {
        let mut state = State {
            fn_name: fn_name,
            env: env,
            branches: vec![],
            asm: vec![Label(name)],
            branch_count: branch_count,
            globals: globals
        };
        for thing in body.iter() {
            state.compile_expr(thing);
        }
        state.push_raw(tail);
        let State { asm, branches, .. } = state;
        (asm, branches)
    }

    fn get_next_label(&mut self, sort: &str) -> String {
        *self.branch_count += 1;
        format!("{}-{}-{}", self.fn_name, sort, *self.branch_count)
    }

    fn push_raw(&mut self, inst: Instruction) {
        self.asm.push(Inst(Raw(inst)))
    }

    fn push(&mut self, inst: NamedInstruction) {
        self.asm.push(Inst(inst))
    }

    fn compile_expr(&mut self, code: &'a AST) {
        match *code {
            Integer(x) => self.push_raw(asm::LDC(x)),
            Atom(ref name) => {
                let new_inst = match self.env.find(name.as_slice()) {
                    Some((level, address)) => asm::LD(level, address),
                    None => {
                        match self.globals.find(&name.as_slice()) {
                            Some(&Const(num)) => asm::LDC(num),
                            Some(&Fun(ref name)) => {
                                self.push(asm::NLDF(name.to_string()));
                                return
                            }
                            None => fail!("no such fn or variable '{}'", name.as_slice())
                        }
                    }
                };
                self.push_raw(new_inst);
            }
            Sequence(ref things) => {
                assert!(!things.is_empty(), "compiling empty sequence");

                let num_args = things.len() - 1;
                let head = &things[0];

                match *head {
                    Atom(ref name) if name.as_slice() == "if" => {
                        // (if cond true false)

                        // compile and push the cond expr
                        self.compile_expr(&things[1]);

                        let label_t = self.get_next_label("branch");
                        let (asm_t, branches_t) =
                            State::compile_section(things.slice(2, 3),
                                                   label_t.clone(),
                                                   self.fn_name.clone(),
                                                   self.env.clone(),
                                                   self.branch_count, self.globals, asm::JOIN);
                        let label_f = self.get_next_label("branch");
                        let (asm_f, branches_f) =
                            State::compile_section(things.slice(3, 4),
                                                   label_f.clone(),
                                                   self.fn_name.clone(),
                                                   self.env.clone(),
                                                   self.branch_count, self.globals, asm::JOIN);

                        self.push(asm::NSEL(label_t, label_f));

                        self.branches.push(asm_t);
                        self.branches.push_all_move(branches_t);
                        self.branches.push(asm_f);
                        self.branches.push_all_move(branches_f);

                        return
                    }
                    Atom(ref name) if name.as_slice() == "set" => {
                        assert!(num_args == 2, "set needs two things, got {}", num_args);

                        let name = match things[1] {
                            Atom(ref name) => name,
                            ref x => fail!("Expected variable name but got {}", *x)
                        };

                        match self.env.find(name.as_slice()) {
                            Some((frame, address)) => {
                                self.compile_expr(&things[2]);
                                self.push_raw(asm::ST(frame, address));
                            }
                            None => fail!("failed to set local variable {}", name)
                        }
                        return
                    }
                    Atom(ref name) if name.as_slice() == "let" => {
                        // (let ((name expression) ...) value)
                        assert!(num_args >= 2, "let needs two things, got {}", num_args);

                        let pairs = match things[1] {
                            Sequence(ref pairs) => {
                                pairs.iter().map(extract_let_pair).collect::<Vec<(&str, &AST)>>()
                            }
                            _ => fail!("let needs pairs, found {}", things[1])
                        };

                        let mut new_frame = HashMap::new();
                        for (i, &(name, body)) in pairs.iter().enumerate() {
                            // evaluate each argument, placing it on the stack
                            self.compile_expr(body);
                            assert!(new_frame.insert(name, i as u32),
                                    "let with duplicated name {}", name)
                        }
                        let let_label = self.get_next_label("let");
                        self.push_raw(asm::DUM(pairs.len() as u32));
                        self.push(asm::NLDF(let_label.clone()));
                        self.push_raw(asm::RAP(pairs.len() as u32));

                        let env = Rc::new(Env {
                            here: new_frame,
                            parent: Some(self.env.clone())
                        });

                        // evaluate all the things
                        let (let_asm, let_branches) =
                            State::compile_section(things.slice_from(2),
                                                   let_label.clone(),
                                                   self.fn_name.clone(),
                                                   env.clone(),
                                                   self.branch_count, self.globals, asm::RTN);
                        self.branches.push(let_asm);
                        self.branches.push_all_move(let_branches);
                        return
                    }
                    Atom(ref name) if name.as_slice() == "lambda" => {
                        // (lambda (x y...) value...)
                        assert!(num_args >= 2, "lambda needs two things, got {}", num_args);

                        let name = self.get_next_label("lambda");
                        let fninfo = parse_fn_decl(name.clone(), &things[1], things.slice_from(2),
                                                   Some(self.env.clone()));
                        let compiled = State::compile_fn(&fninfo, self.branch_count, self.globals);
                        self.branches.push(compiled);
                        self.push(asm::NLDF(name));
                        return
                    }
                    _ => {}
                }

                // compile everything except the function.
                for thing in things.tail().iter() {
                    self.compile_expr(thing)
                }

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
                            "do" | "pass" => {
                                return
                            }
                            "+" | "*" => {
                                // (add a b c d ...)

                                let is_mul = head == "*";

                                if num_args == 0 {
                                    self.push_raw(asm::LDC(is_mul as i32))
                                } else {
                                    let inst = if is_mul {asm::MUL} else {asm::ADD};

                                    for _ in range(0, num_args - 1) {
                                        self.push_raw(inst)
                                    }
                                }
                                return
                            }
                            "-" => x!(SUB, 2),
                            "/" => x!(DIV, 2),
                            "cons" => x!(CONS, 2),
                            "car" => x!(CAR, 1),
                            "cdr" => x!(CDR, 1),
                            "atom" => x!(ATOM, 1),
                            "brk" => x!(BRK, 0),
                            "dbug" => x!(DBUG, 1),
                            "=" => x!(CEQ, 2),
                            ">=" => x!(CGTE, 2),
                            ">" => x!(CGT, 2),
                            _ => None
                        };
                        match inst_args {
                            Some((inst, count)) => {
                                assert!(num_args == count,
                                        "{} expects exactly {} args, but found {}",
                                        head, count, num_args);

                                self.push_raw(inst);
                                return
                            }
                            None => {/* default */}
                        }
                    }
                    _ => {/* default */}
                }
                // compile the function
                self.compile_expr(&things[0]);
                self.push_raw(asm::AP(num_args as u32));
            }
        }
    }
}

fn extract_let_pair<'a>(p: &'a AST) -> (&'a str, &'a AST) {
    match *p {
        Sequence(ref name_expr) => {
            match name_expr.as_slice() {
                [Atom(ref name), ref result] => {
                    (name.as_slice(), result)
                }
                _ => {
                    fail!("`let` needs name-expression pair, found {}", p)
                }
            }
        }
        _ => fail!("`let` needs pair, found {}", p)
    }
}
