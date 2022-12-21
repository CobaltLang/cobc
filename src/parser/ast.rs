#[derive(Debug)]
pub enum AstNode {
    Stmnt(Stmnt),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Stmnt {
    Declare {
        ident: String,
        type_ident: String,
        value: Box<Expr>,
    },

    Assign {
        ident: String,
        value: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Expr {
    Const(Const),
    Ident(String),

    UnaryOp {
        op: Op,
        rs: Box<Expr>,
    },

    BinOp {
        ls: Box<Expr>,
        op: Op,
        rs: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Const {
    Float(f64),
    Int(usize),
    Chr(char),
    Str(String),
}

#[derive(Debug)]
pub enum Op {
    // Unary
    Neg,

    // Binary
    Add,
    Sub,
    Mul,
    Div,
}