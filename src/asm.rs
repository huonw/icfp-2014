#[deriving(Show)]
pub enum Instruction {
    LDC(u32),
    LD(u32, u32),
    ADD,
    SUB,
    MUL,
    DIV,
    CEQ,
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
