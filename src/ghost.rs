use regex::{mod, Regex};

use std::io::File;

use std::collections::HashMap;
use std::fmt::{mod, Show, Formatter};
use std::from_str::FromStr;

macro_rules! opt_chain {
    ($str: expr => $($wrapper: ident)|*) => {
        None
            $(.or_else(|| from_str($str).map($wrapper)))*
    }
}

#[deriving(Clone)]
struct Variable {
    name: String
}
impl FromStr for Variable {
    fn from_str(x: &str) -> Option<Variable> {
        if x.starts_with("@") {
            Some(Variable { name: x.slice_from(1).to_string() })
        } else {
            None
        }
    }
}
impl Show for Variable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!("@".fmt(f));
        self.name.fmt(f)
    }
}
#[deriving(Clone)]
struct Constant {
    name: String
}
impl FromStr for Constant {
    fn from_str(x: &str) -> Option<Constant> {
        if x.starts_with("$") {
            Some(Constant { name: x.slice_from(1).to_string() })
        } else {
            None
        }
    }
}
impl Show for Constant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!("$".fmt(f));
        self.name.fmt(f)
    }
}

#[deriving(Clone)]
struct Label {
    name: String
}
impl FromStr for Label {
    fn from_str(x: &str) -> Option<Label> {
        Some(Label { name: x.to_string() })
    }
}
impl Show for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.name.fmt(f)
    }
}


#[deriving(Clone)]
enum GPRegister {
    A, B, C, D, E, F, G, H
}

impl FromStr for GPRegister {
    fn from_str(x: &str) -> Option<GPRegister> {
        Some(match x {
            "a" => A,
            "b" => B,
            "c" => C,
            "d" => D,
            "e" => E,
            "f" => F,
            "g" => G,
            "h" => H,
            _ => return None
        })
    }
}

impl Show for GPRegister {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        (match *self {
            A => "a", B => "b", C => "c", D => "d",
            E => "e", F => "f", G => "g", H => "h"
        }).fmt(f)
    }
}

#[deriving(Clone)]
enum Address {
    AddrReg(GPRegister),
    AddrConst(u8)
}

impl FromStr for Address {
    fn from_str(x: &str) -> Option<Address> {
        if x.starts_with("[") && x.ends_with("]") {
            let inner = x.slice(1, x.len() - 1);
            opt_chain!(inner => AddrReg | AddrConst)
        } else {
            None
        }
    }
}
impl Show for Address {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!("[".fmt(f));
        try!(match *self {
            AddrReg(ref r) => r.fmt(f),
            AddrConst(ref c) => c.fmt(f),
        })
        "]".fmt(f)
    }
}

#[deriving(Clone)]
enum Writable {
    WrReg(GPRegister),
    WrAddr(Address),
    WrVariable(Variable),
    WrPC,
}

impl FromStr for Writable {
    fn from_str(x: &str) -> Option<Writable> {
        if x == "pc" {
            Some(WrPC)
        } else {
            opt_chain!(x => WrReg | WrAddr | WrVariable)
        }
    }
}
impl Show for Writable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            WrReg(ref r) => r.fmt(f),
            WrAddr(ref a) => a.fmt(f),
            WrVariable(ref v) => v.fmt(f),
            WrPC => "pc".fmt(f)
        }
    }
}

#[deriving(Clone)]
enum Readable {
    ReWr(Writable),
    ReVariableAddr(Variable),
    ReConst(u8),
    ReConstant(Constant),
    ReLabel(Label)
}

impl FromStr for Readable {
    fn from_str(x: &str) -> Option<Readable> {
        if x.starts_with("&") {
            from_str(x.slice_from(1).trim()).map(ReVariableAddr)
        } else {
            opt_chain!(x => ReWr | ReConst | ReConstant | ReLabel)
        }
    }
}
impl Show for Readable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ReWr(ref w) => w.fmt(f),
            ReConst(ref c) => c.fmt(f),
            ReConstant(ref c) => c.fmt(f),
            ReVariableAddr(ref v) => {
                try!("&".fmt(f));
                v.fmt(f)
            }
            ReLabel(ref l) => l.fmt(f),
        }
    }
}

#[deriving(Clone)]
enum Interrupt {
    IntLit(u8),
    IntConstant(Constant)
}

impl FromStr for Interrupt {
    fn from_str(x: &str) -> Option<Interrupt> {
        opt_chain!(x => IntLit | IntConstant)
    }
}
impl Show for Interrupt {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            IntLit(ref l) => l.fmt(f),
            IntConstant(ref c) => c.fmt(f)
        }
    }
}

#[deriving(Clone)]
enum PositionOrLabel {
    Pos(u8),
    PosLabel(Label)
}
impl PositionOrLabel {
    fn into_readable(self) -> Readable {
        match self {
            Pos(x) => ReConst(x),
            PosLabel(l) => ReLabel(l)
        }
    }
}

impl FromStr for PositionOrLabel {
    fn from_str(x: &str) -> Option<PositionOrLabel> {
        opt_chain!(x => Pos | PosLabel)
    }
}

impl Show for PositionOrLabel {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Pos(ref c) => c.fmt(f),
            PosLabel(ref s) => s.fmt(f)
        }
    }
}

#[deriving(Clone)]
#[allow(non_camel_case_types)]
enum Instruction {
    MOV(Writable, Readable),
    INC(Writable),
    DEC(Writable),
    ADD(Writable, Readable),
    SUB(Writable, Readable),
    MUL(Writable, Readable),
    DIV(Writable, Readable),
    AND(Writable, Readable),
    OR(Writable, Readable),
    XOR(Writable, Readable),
    JLT(PositionOrLabel, Readable, Readable),
    JEQ(PositionOrLabel, Readable, Readable),
    JGT(PositionOrLabel, Readable, Readable),

    INT(Interrupt),
    HLT,

    PUSH(Readable),
    POP(Writable),
    JUMP(PositionOrLabel),
    CALL(PositionOrLabel),
    CALL_RET_TO(PositionOrLabel, PositionOrLabel),
    RETURN,
    DEBUG
}

impl FromStr for Instruction {
    fn from_str(x: &str) -> Option<Instruction> {
        let mut words = x.split(|c: char| c == ',' || c.is_whitespace()).filter(|s| !s.is_empty());

        let first = match words.next() {
            Some(f) => f,
            None => return None
        };
        macro_rules! opt_ap {
            (: $var: ident $($name: ident)*) => {
                $var(
                    $(match words.next().and_then(from_str) {
                        Some($name) => $name,
                        None => return None
                    }),*)
            };
            ($var: ident 0) => {
                $var
            };
            ($var: ident 1) => {
                opt_ap!(: $var x)
            };
            ($var: ident 2) => {
                opt_ap!(: $var x y)
            };
            ($var: ident 3) => {
                opt_ap!(: $var x y z)
            }
        };
        macro_rules! parse {
            ($($name: pat => $ctor:ident($num:tt)),*) => {
                match first {
                    $($name => opt_ap!($ctor $num),)*
                        _ => return None
                }
            }
        }

        Some(parse! {
            "mov" => MOV(2),
            "inc" => INC(1),
            "dec" => DEC(1),
            "add" => ADD(2),
            "sub" => SUB(2),
            "mul" => MUL(2),
            "div" => DIV(2),
            "and" => ADD(2),
            "or" => OR(2),
            "xor" => XOR(2),
            "jlt" => JLT(3),
            "jeq" => JEQ(3),
            "jgt" => JGT(3),
            "int" => INT(1),
            "hlt" => HLT(0),

            "push" => PUSH(1),
            "pop" => POP(1),
            "call" => CALL(1),
            "call-ret-to" => CALL_RET_TO(2),
            "jump" => JUMP(1),
            "return" => RETURN(0),
            "debug" => DEBUG(0)

        })
    }
}

impl Show for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        macro_rules! show_work {
            ($string: expr $x:ident $($xs:ident)*) => {
                {
                    try!($string.fmt(f));
                    try!(" ".fmt(f));
                    try!($x.fmt(f));
                    $(
                        try!(", ".fmt(f));
                        try!($xs.fmt(f));
                        )*
                }
            }
        }

        macro_rules! show {
            ($($name: ident($($x:ident),+) => $shown: expr),*) => {
                match *self {
                    $($name($(ref $x),+) => show_work!($shown $($x)+),)*
                        // special case because lazy.
                        HLT => try!("hlt".fmt(f)),
                    RETURN => try!("return".fmt(f)),
                    DEBUG => try!("debug".fmt(f))
                }
            }
        }
        show! {
            MOV(x,y) => "mov",
            INC(x) => "inc",
            DEC(x) => "dec",
            ADD(x,y) => "add",
            SUB(x,y) => "sub",
            MUL(x,y) => "mul",
            DIV(x,y) => "div",
            AND(x,y) => "and",
            OR(x,y) => "or",
            XOR(x,y) => "xor",
            JLT(x,y,z) => "jlt",
            JEQ(x,y,z) => "jeq",
            JGT(x,y,z) => "jgt",
            INT(x) => "int",

            PUSH(x) => "push",
            POP(x) => "pop",
            CALL(x) => "call",
            CALL_RET_TO(x,y) => "call-ret-to",
            JUMP(x) => "lab"
        }
        Ok(())
    }
}

#[deriving(Clone)]
enum LabelOrInstruction {
    Inst(Instruction),
    Label(Label)
}

enum BinOp {
    BMov, BAdd, BSub, BMul, BDiv, BAnd, BOr, BXor
}
impl BinOp {
    fn construct(&self, lhs: Writable, rhs: Readable) -> Instruction {
        (match *self {
            BMov => MOV, BAdd => ADD, BSub => SUB, BMul => MUL,
            BDiv => DIV, BAnd => AND, BOr => OR, BXor => XOR
        })(lhs, rhs)
    }
}
impl FromStr for BinOp {
    fn from_str(x: &str) -> Option<BinOp> {
        Some(match x {
            "=" => BMov,
            "+=" => BAdd,
            "-=" => BSub,
            "*=" => BMul,
            "/=" => BDiv,
            "&=" => BAnd,
            "|=" => BOr,
            "^=" => BXor,
            _ => return None
        })
    }
}

impl FromStr for LabelOrInstruction {
    fn from_str(x: &str) -> Option<LabelOrInstruction> {
        let mut w = x.words();
        match (w.next(), w.next()) {
            (Some(lab), None) if lab.ends_with(":") => {
                Some(Label(Label { name: lab.slice_to(lab.len() - 1).to_string() }))
            }
            _ => from_str(x).map(Inst)
        }
    }
}

pub fn parse(code: &str) -> (Vec<LabelOrInstruction>,
                         HashMap<String, u8>,
                         HashMap<String, u8>) {
    static INCLUDES: Regex = regex!("include\\s*\"([^\"]*)\"");
    let code = INCLUDES.replace_all(code.as_slice(), |captures: &regex::Captures| {
        let filename = captures.at(1);
        // errors, what errors?
        File::open(&Path::new(filename)).unwrap().read_to_string().unwrap()
    });

    static COMMENTS: Regex = regex!(";.*");
    let code = COMMENTS.replace_all(code.as_slice(), "");

    static OPERATORS: Regex = regex!(r"[+*/&^|-]?=");
    let code = OPERATORS.replace_all(code.as_slice(), " $0 ");


    let mut variables = HashMap::new();
    let mut variable_index = 0u8;

    let mut constants = HashMap::new();
    let mut parsed = vec![];

    let mut lines = code.as_slice().lines_any();
    let mut initial = true;

    for line in lines {
        let line = line.trim();
        if line.is_empty() { continue }

        let mut w = line.words();
        match (w.next(), w.next(), w.next(), w.next()) {
            (Some("decl"), Some(var), None, _) => {
                if initial {
                    assert!(variables.insert(var.to_string(), variable_index),
                            "duplicated decl of variable {}: {}", var, line);
                    variable_index += 1
                } else {
                    fail!("decls only allowed at the top of a file: {}", line)
                }
                continue
            }
            (Some("const"), Some(name), Some(value), None) => {
                match from_str::<u8>(value) {
                    Some(v) => assert!(constants.insert(name.to_string(), v),
                                       "duplicated constant {}: {}", name, line),
                    None => fail!("constant with invalid value {}: {}", name, line)
                }
                continue
            }
            (Some(lhs), Some(op), Some(rhs), None) => {
                match (from_str(lhs), from_str::<BinOp>(op), from_str(rhs)) {
                    (Some(l), Some(b_op), Some(r)) => {
                        parsed.push(Inst(b_op.construct(l, r)));
                        continue
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        initial = false;

        match from_str::<LabelOrInstruction>(line) {
            Some(x) => parsed.push(x),
            None => fail!("could not parse {}", line)
        }
    }

    (parsed, variables, constants)
}

trait Resolve {
    fn resolve(&self,
               code: &Instruction,
               variables: &HashMap<String, u8>,
               constants: &HashMap<String, u8>,
               labels: &HashMap<String, u8>) -> Self;
}

fn resolve_variable(var: &Variable,
                    code: &Instruction,
                    variables: &HashMap<String, u8>) -> u8 {
    match variables.find(&var.name) {
        Some(&val) => val,
        None => fail!("unknown variable {}: {}", *var, *code)
    }
}

impl Resolve for Writable {
    fn resolve(&self,
               code: &Instruction,
               variables: &HashMap<String, u8>,
               _constants: &HashMap<String, u8>,
               _labels: &HashMap<String, u8>) -> Writable {
        match *self {
            WrAddr(_) | WrReg(_) | WrPC => self.clone(),
            WrVariable(ref name) => WrAddr(AddrConst(resolve_variable(name, code, variables)))
        }
    }
}
fn resolve_label(lab: &Label,
                 code: &Instruction,
                 labels: &HashMap<String, u8>) -> u8 {
    match labels.find(&lab.name) {
        Some(&val) => val,
        None => fail!("unknown label {}: {}", lab.name, *code)
    }
}

impl Resolve for Readable {
    fn resolve(&self,
               code: &Instruction,
               variables: &HashMap<String, u8>,
               constants: &HashMap<String, u8>,
               labels: &HashMap<String, u8>) -> Readable {
        match *self {
            ReConst(_) => self.clone(),
            ReWr(ref w) => ReWr(w.resolve(code, variables, constants, labels)),
            ReVariableAddr(ref v) => ReConst(resolve_variable(v, code, variables)),
            ReLabel(ref l) => ReConst(resolve_label(l, code, labels)),
            ReConstant(ref name) => {
                match constants.find(&name.name) {
                    Some(&val) => ReConst(val),
                    None => fail!("unknown constant {}: {}", *name, *code)
                }
            }
        }
    }
}

impl Resolve for PositionOrLabel {
    fn resolve(&self,
               code: &Instruction,
               _variables: &HashMap<String, u8>,
               _constants: &HashMap<String, u8>,
               labels: &HashMap<String, u8>) -> PositionOrLabel {
        match *self {
            Pos(x) => Pos(x),
            PosLabel(ref name) => Pos(resolve_label(name, code, labels))
        }
    }
}
impl Resolve for Interrupt {
    fn resolve(&self,
               code: &Instruction,
               _variables: &HashMap<String, u8>,
               constants: &HashMap<String, u8>,
               _labels: &HashMap<String, u8>) -> Interrupt {
        // allow this to be naturally used in the generic macro below
        match *self {
            IntLit(x) => IntLit(x),
            IntConstant(ref name) => {
                match constants.find(&name.name) {
                    Some(&val) => IntLit(val),
                    None => fail!("unknown constant {}: {}", *name, *code)
                }
            }
        }
    }
}
pub fn compile(code: &[LabelOrInstruction],
               variables: &HashMap<String, u8>, constants: &HashMap<String, u8>)
               -> Vec<Instruction> {
    // initialise the stack pointer
    let mut no_sugar = vec![Inst(MOV(WrReg(H), ReConst(255)))];
    for thing in code.iter() {
        match *thing {
            Label(_) => no_sugar.push(thing.clone()),
            Inst(ref i) => desugar(i.clone(), &mut no_sugar)
        }
    }
    let mut labels = HashMap::new();
    let mut index = 0u;
    for thing in no_sugar.iter() {
        match *thing {
            Label(ref name) => {
                assert!(labels.insert(name.to_string(), index as u8),
                        "duplicated label {}", name)
            }
            Inst(_) => index += 1,
        }
    }

    assert!(variables.len() <= 256, "program has {} variables, limit is 256", variables.len());

    let mut ret = vec![];
    for thing in no_sugar.iter() {
        let inst = match *thing {
            Label(_) => continue,
            Inst(ref inst) => inst
        };

        macro_rules! resolve {
            ($($name: ident($($x:ident),+)),*) => {
                match *inst {
                    $($name($(ref $x),+) => {
                        $name($($x.resolve(inst, variables, constants, &labels) ),*)
                    })*
                        // special case because lazy.
                        HLT => HLT,
                    RETURN => RETURN,
                    DEBUG => DEBUG,
                }
            }
        }
        let resolved = resolve! {
            MOV(x,y),
            INC(x),
            DEC(x),
            ADD(x,y),
            SUB(x,y),
            MUL(x,y),
            DIV(x,y),
            AND(x,y),
            OR(x,y),
            XOR(x,y),
            JLT(x,y,z),
            JEQ(x,y,z),
            JGT(x,y,z),
            INT(x),

            PUSH(x),
            POP(x),
            CALL(x),
            CALL_RET_TO(x,y),
            JUMP(lab)
        };

        ret.push(resolved);
    }

    assert!(ret.len() <= 256, "program has {} instructions, limit is 256", ret.len());
    ret
}

static CURR_STACK: Writable = WrAddr(AddrReg(H));
static CURR_STACK_R: Readable = ReWr(CURR_STACK);

fn desugar(inst: Instruction, asm: &mut Vec<LabelOrInstruction>) {
    macro_rules! desugar {
        ($arg: expr) => { desugar($arg, asm) }
    }
    macro_rules! inst {
        ($arg: expr) => { asm.push(Inst($arg)) }
    }
    match inst {
        PUSH(val) => {
            // [h] = val
            // dec h
            inst!(MOV(CURR_STACK.clone(), val));
            inst!(DEC(WrReg(H)));
        }
        POP(val) => {
            // inc h
            // val = [h]
            inst!(INC(WrReg(H)));
            inst!(MOV(val, CURR_STACK_R.clone()));
        }
        JUMP(lab) => {
            inst!(MOV(WrPC, lab.into_readable()))
        }
        CALL(lab) => {
            let ret_label = Label { name: format!("ret-label-{}", asm.len()) };
            desugar!(CALL_RET_TO(lab, PosLabel(ret_label.clone())));
            asm.push(Label(ret_label));
        }
        CALL_RET_TO(func, lab) => {
            desugar!(PUSH(lab.into_readable()));
            desugar!(JUMP(func));
        }
        RETURN => {
            desugar!(POP(WrPC))
        }
        DEBUG => {
            inst!(INT(IntLit(8)))
        }
        _ => inst!(inst.clone())
    }
}
