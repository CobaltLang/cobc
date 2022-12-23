pub mod ast;
pub mod checks;

extern crate pest;

use pest::{iterators::Pair, pratt_parser::PrattParser, Parser};
use pest_derive::Parser;

use ast::*;

// Pratt parser for expressions
lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence from lowest to highest
        PrattParser::new()
            .op(Op::infix(add, Left) | Op::infix(sub, Left))
            .op(Op::infix(mul, Left) | Op::infix(div, Left))
            .op(Op::prefix(neg))

    };
}

pub fn parse(source: &str) -> Vec<AstNode> {
    let mut parsed = CobaltParser::parse(Rule::program, source)
        .expect("Parse failed")
        .next()
        .unwrap()
        .into_inner();

    let mut nodes = Vec::new();

    for block in parsed {
        let block = CobaltParser::parse_block(block);

        block.check_vars(&Vec::new());
        block.check_types(&Vec::new(), None);

        nodes.push(AstNode::Block(block))
    }

    nodes
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct CobaltParser;

impl CobaltParser {
    fn parse_block(block: Pair<Rule>) -> Block {
        let inner = block.into_inner();

        let mut parsed_block = Block {
            stmnts: Vec::new(),
            expr: None,
        };

        for pair in inner {
            match pair.as_rule() {
                Rule::stmnt => parsed_block
                    .stmnts
                    .push(CobaltParser::parse_stmnt(pair.into_inner().next().unwrap())),

                // A lone expression will always mark the end of a block
                Rule::expr => parsed_block.expr = Some(CobaltParser::parse_expr(pair)),

                other => unreachable!("Parser expected statement or expr, found {:?}", other),
            }
        }

        parsed_block
    }

    fn parse_stmnt(stmnt: Pair<Rule>) -> Stmnt {
        match stmnt.as_rule() {
            Rule::declStmnt => {
                let mut inner = stmnt.into_inner();

                let ident = inner.next().unwrap().as_str().to_owned();

                // Next token could either the type's name or the assigned value
                let next = inner.next().unwrap();

                match next.as_rule() {
                    Rule::ident => Stmnt::Declare {
                        ident,
                        type_ident: next.as_str().to_owned(),
                        value: Box::new(CobaltParser::parse_expr(inner.next().unwrap())),
                    },

                    // Untyped decl, infer type later
                    Rule::expr => Stmnt::Declare {
                        ident,
                        type_ident: "".to_string(),
                        value: Box::new(CobaltParser::parse_expr(next)),
                    },

                    other => unreachable!("Parser expected type or expr, found {:?}", other),
                }
            }

            Rule::assignStmnt => {
                let mut inner = stmnt.into_inner();

                Stmnt::Assign {
                    ident: inner.next().unwrap().as_str().to_owned(),
                    value: Box::new(CobaltParser::parse_expr(inner.next().unwrap())),
                }
            }

            other => unreachable!("Parser expected statement, found {:?}", other),
        }
    }

    fn parse_expr(expr: Pair<Rule>) -> Expr {
        PRATT_PARSER
            .map_primary(|p| match p.as_rule() {
                // Evaluate the optional Expr at the end of the block
                Rule::block => {
                    // clone required
                    let expr = p.clone().into_inner().last().unwrap();

                    // Check the block ends with an expression
                    match expr.as_rule() {
                        Rule::expr => Expr::Block(Box::new(CobaltParser::parse_block(p))),

                        other => {
                            unreachable!("Parser expected expr at end of block, found {:?}", other)
                        }
                    }
                }

                Rule::expr => CobaltParser::parse_expr(p),

                // Constants
                Rule::float => Expr::Const(Const::Float(p.as_str().parse().unwrap())),

                // 0b = binary | 0o = octal | 0h = hexadecimal
                Rule::int => {
                    let inner = p.into_inner().next().unwrap();

                    match inner.as_rule() {
                        Rule::decConst => Expr::Const(Const::Int(inner.as_str().parse().unwrap())),

                        Rule::hexConst => {
                            let val = inner.as_str().strip_prefix("0h").unwrap();

                            Expr::Const(Const::Int(usize::from_str_radix(val, 16).unwrap()))
                        }

                        Rule::octConst => {
                            let val = inner.as_str().strip_prefix("0o").unwrap();

                            Expr::Const(Const::Int(usize::from_str_radix(val, 8).unwrap()))
                        }

                        Rule::binConst => {
                            let val = inner.as_str().strip_prefix("0b").unwrap();

                            Expr::Const(Const::Int(usize::from_str_radix(val, 2).unwrap()))
                        }

                        other => unreachable!("Unknown constant prefix: {:?}", other),
                    }
                }

                // Chars and strings need to have their inner values parsed,
                // this ignores the outer "" / ''
                Rule::chr => Expr::Const(Const::Chr(
                    p.into_inner().next().unwrap().as_str().parse().unwrap(),
                )),
                Rule::str => Expr::Const(Const::Str(
                    p.into_inner().next().unwrap().as_str().parse().unwrap(),
                )),

                Rule::ident => Expr::Ident(p.as_str().to_owned()),

                other => unreachable!("Parser expected atom, found {:?}", other),
            })
            .map_prefix(|op, rs| match op.as_rule() {
                Rule::neg => Expr::UnaryOp {
                    op: Op::Neg,
                    rs: Box::new(rs),
                },
                other => unreachable!("Parser expected unary op, found {:?}", other),
            })
            .map_infix(|ls, op, rs| {
                let op = match op.as_rule() {
                    Rule::add => Op::Add,
                    Rule::sub => Op::Sub,
                    Rule::div => Op::Div,
                    Rule::mul => Op::Mul,
                    other => unreachable!("Parser expected binary operator, found {:?}", other),
                };

                Expr::BinOp {
                    ls: Box::new(ls),
                    op,
                    rs: Box::new(rs),
                }
            })
            .parse(expr.into_inner())
    }
}
