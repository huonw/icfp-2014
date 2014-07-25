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
