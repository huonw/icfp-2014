#[deriving(Show, Eq, PartialEq)]
pub enum AST {
    Atom(String),
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
            _ => { curr.push(Atom(word.to_string())) }
        }
    }

    assert!(stack.is_empty(), "too many opening parentheses: {}\n{}", stack, code);

    Sequence(curr)
}