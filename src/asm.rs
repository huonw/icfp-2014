use std::collections::HashMap;

#[deriving(Show)]
pub enum Instruction {
    LDC(i32),
    LD(u32, u32),
    ADD,
    SUB,
    MUL,
    DIV,
    CEQ,
    CGT,
    CGTE,
    ATOM,
    CONS,
    CAR,
    CDR,
    SEL(u32, u32),
    JOIN,
    LDF(u32),
    AP(u32),
    RTN,
    DUM(u32),
    RAP(u32),
    STOP,
    TSEL(u32, u32),
    TAP(u32),
    TRAP(u32),
    ST(u32, u32),
    DBUG,
    BRK
}

#[deriving(Show)]
pub enum NamedInstruction {
    Raw(Instruction),
    NSEL(String, String),
    NTSEL(String, String),
    NLDF(String)
}
#[deriving(Show)]
pub enum LabelOrInstruction {
    Label(String),
    Inst(NamedInstruction)
}

#[allow(dead_code)]
pub fn parse(s: &str) -> Vec<LabelOrInstruction> {
    let mut result = vec![];

    let mut splits = s.words();

    macro_rules! no_args {
        ($value: expr) => {
            {
                result.push(Inst(Raw($value)));
                continue
            }
        }
    }
    macro_rules! one_named_arg {
        ($value: ident) => {
            {
                result.push(Inst($value(splits.next().unwrap().to_string())));
                continue
            }
        }
    }
    macro_rules! two_named_args {
        ($value: ident) => {
            {
                result.push(Inst($value(splits.next().unwrap().to_string(),
                                        splits.next().unwrap().to_string())));
                continue
            }
        }
    }

    macro_rules! one_number_arg {
        ($value: ident) => {
            {
                result.push(Inst(Raw($value(splits.next().and_then(from_str).unwrap()))));
                continue
            }
        }
    }
    macro_rules! two_number_args {
        ($value: ident) => {
            {
                result.push(Inst(Raw($value(splits.next().and_then(from_str).unwrap(),
                                            splits.next().and_then(from_str).unwrap()))));
                continue
            }
        }
    }

    loop {
        let part = match splits.next() {
            Some(p) => p,
            None => break
        };
        if part.ends_with(":") {
            result.push(Label(part.slice_to(part.len() - 1).to_string()));
            continue
        }

        match part {
            "ADD" => no_args!(ADD),
            "SUB" => no_args!(SUB),
            "MUL" => no_args!(MUL),
            "DIV" => no_args!(DIV),
            "CEQ" => no_args!(CEQ),
            "CGT" => no_args!(CGT),
            "CGTE" => no_args!(CGTE),
            "ATOM" => no_args!(ATOM),
            "CONS" => no_args!(CONS),
            "CAR" => no_args!(CAR),
            "CDR" => no_args!(CDR),
            "JOIN" => no_args!(JOIN),
            "STOP" => no_args!(STOP),
            "DBUG" => no_args!(DBUG),
            "BRK" => no_args!(BRK),
            "LDF" => one_named_arg!(NLDF),
            "SEL" => two_named_args!(NSEL),
            "TSEL" => two_named_args!(NTSEL),
            "TAP" => one_number_arg!(TAP),
            "TRAP" => one_number_arg!(TRAP),
            "RAP" => one_number_arg!(RAP),
            "AP" => one_number_arg!(AP),
            "DUM" => one_number_arg!(DUM),
            "ST" => two_number_args!(ST),
            "LD" => two_number_args!(LD),
            "LDC" => one_number_arg!(LDC),
            _ => fail!("bad instruction: {}", part),
        }
    }
    return result;
}

pub fn compile(queue: &[LabelOrInstruction]) -> Vec<Instruction> {
    let mut labels = HashMap::new();
    let mut instruction_count = 0u32;
    let mut ret = vec![];

    for inst_or_label in queue.iter() {
        match *inst_or_label {
            Label(ref label) => { labels.insert(label.as_slice(), instruction_count); }
            _ => instruction_count += 1,
        }
    }

    for inst_or_label in queue.iter() {
        match *inst_or_label {
            Label(_) => {}
            Inst(Raw(inst)) => ret.push(inst),
            Inst(NSEL(ref x, ref y)) => ret.push(SEL(*labels.get(&x.as_slice()), *labels.get(&y.as_slice()))),
            Inst(NTSEL(ref x, ref y)) => ret.push(TSEL(*labels.get(&x.as_slice()), *labels.get(&y.as_slice()))),
            Inst(NLDF(ref x)) => ret.push(LDF(*labels.get(&x.as_slice()))),
        }
    }

    ret
}
pub fn print_inst(inst: &Instruction) -> String {
    match *inst {
        LDC(x) => format!("LDC {}", x),
        LDF(x) => format!("LDF {}", x),
        AP(x) => format!("AP {}", x),
        SEL(x, y) => format!("SEL {} {}", x, y),
        LD(x, y) => format!("LD {} {}", x, y),
        DUM(x) => format!("DUM {}", x),
        RAP(x) => format!("RAP {}", x),
        ST(x, y) => format!("ST {} {}", x, y),
        TSEL(x, y) => format!("TSEL {} {}", x, y),
        TRAP(x) => format!("TRAP {}", x),
        _ => inst.to_string(),
    }
}
pub fn print(code: &[Instruction]) -> String {
    let mut ret = String::new();

    for inst in code.iter() {
        ret.push_str(print_inst(inst).as_slice());
        ret.push_str("\n");
    }
    ret
}

pub fn print_label_or_inst(lab_or_inst: &LabelOrInstruction) -> String {
    match *lab_or_inst {
        Label(ref l) => format!("{}:", *l),
        Inst(NSEL(ref x, ref y)) => format!("   SEL {} {}", *x, *y),
        Inst(NTSEL(ref x, ref y)) => format!("   TSEL {} {}", *x, *y),
        Inst(NLDF(ref x)) => format!("   LDF {}", *x),
        Inst(Raw(inst)) => format!("   {}", print_inst(&inst))
    }
}
pub fn print_labelled(code: &[LabelOrInstruction]) -> String {
    let mut ret = String::new();

    for lab_or_inst in code.iter() {
        ret.push_str(print_label_or_inst(lab_or_inst).as_slice());
        ret.push_str("\n")
    }
    ret
}
