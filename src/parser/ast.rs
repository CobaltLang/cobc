#[derive(Debug)]
pub enum AstNode {
    Block(Block),
}

// Basic unit of scope - Curly braces {}
#[derive(Debug)]
pub struct Block {
    pub stmnts: Vec<Stmnt>,

    // All blocks can end with an expression
    pub expr: Option<Expr>,
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

    Block(Box<Block>),

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
