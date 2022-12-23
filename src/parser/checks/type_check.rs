use colored::*;

use crate::parser::ast::*;

struct Type;

impl Type {
    // Unsigned integers
    const U1: &'static str = "U1"; // Unsigned 1-byte int - u8
    const U2: &'static str = "U2"; // Unsigned 2-byte int - u16
    const U4: &'static str = "U4"; // Unsigned 4-byte int - u32
    const U8: &'static str = "U8"; // Unsigned 8-byte int - u64
    const UINT: &'static str = "UInt"; // Unsinged int of arch-pointer size - usize

    // Signed integers
    const S1: &'static str = "S1"; // Signed 1-byte int - u8
    const S2: &'static str = "S2"; // Signed 2-byte int - u16
    const S4: &'static str = "S4"; // Signed 4-byte int - u32
    const S8: &'static str = "S8"; // Signed 8-byte int - u64
    const SINT: &'static str = "SInt"; // Singed int of arch-pointer size - usize

    const F4: &'static str = "F4"; // 4-byte float - f32
    const F8: &'static str = "F8"; // 8-byte float - f64

    const CHAR: &'static str = "Char";

    const STRING: &'static str = "String";

    const EMPTY: &'static str = "()";
}

#[derive(Clone)]
pub struct Var {
    pub id: String,
    pub t_id: String,
}

impl Var {
    fn new(id: String, t_id: String) -> Self {
        Self { id, t_id }
    }
}

impl Block {
    pub fn check_types(&self, ext: &Vec<Var>, exp_type: Option<String>) -> Option<String> {
        // Bring all the variables defined in
        // the external scope into scope
        let mut table = ext.clone();

        // Checks that an expression matches a type, or
        // returns the type of the expression (no type
        // supplied)
        fn check_or_find_type(expr: &Expr, table: &Vec<Var>, exp_type: Option<String>) -> String {
            let mut exp_type = exp_type;

            match expr {
                Expr::Const(c) => {
                    // This is the first const/var found
                    if exp_type.is_none() {
                        exp_type = Some(match c {
                            Const::Float(_) => Type::F4.to_owned(),
                            Const::Int(_) => Type::SINT.to_owned(),
                            Const::Chr(_) => Type::CHAR.to_owned(),
                            Const::Str(_) => Type::STRING.to_owned(),
                        })
                    } else {
                        // Check that the types match
                        match c {
                            Const::Float(f) => {
                                let t = exp_type.as_ref().unwrap();

                                if !(t == Type::F4 || t == Type::F8) {
                                    Block::throw_type_err(&f.to_string(), &t);
                                }
                            }

                            Const::Int(i) => {
                                let t = exp_type.as_ref().unwrap();

                                if !(t == Type::U1
                                    || t == Type::U2
                                    || t == Type::U4
                                    || t == Type::U8
                                    || t == Type::S1
                                    || t == Type::S2
                                    || t == Type::S4
                                    || t == Type::SINT
                                    || t == Type::UINT)
                                {
                                    Block::throw_type_err(&i.to_string(), &t);
                                }
                            }

                            Const::Chr(c) => {
                                let t = exp_type.as_ref().unwrap();

                                if t != Type::CHAR {
                                    Block::throw_type_err(&c.to_string(), &t);
                                }
                            }

                            Const::Str(s) => {
                                let t = exp_type.as_ref().unwrap();

                                if t != Type::STRING {
                                    Block::throw_type_err(&s, &t);
                                }
                            }
                        }
                    }
                }

                Expr::Ident(id) => {
                    if exp_type.is_none() {
                        // Expected type is the type of this identifier
                        for var in table {
                            if &var.id == id {
                                exp_type = Some(var.t_id.clone());
                            }
                        }
                    } else {
                        let t = exp_type.as_ref().unwrap();

                        // Find the identifier in the var table
                        for var in table {
                            if &var.id == id {
                                if t != &var.t_id {
                                    Block::throw_type_err(&id, &t);
                                }
                            }
                        }
                    }
                }

                Expr::Block(block) => {
                    if exp_type.is_none() {
                        if let Some(t) = block.check_types(table, None) {
                            exp_type = Some(t);
                        } else {
                            exp_type = Some(Type::EMPTY.to_string())
                        }
                    } else {
                        let t = exp_type.as_ref().unwrap();

                        if let Some(block_type) = block.check_types(table, None) {
                            if &block_type != t {
                                Block::throw_type_err("block", &t);
                            }
                        } else {
                            Block::throw_type_err("block", &t);
                        }
                    }
                },

                Expr::UnaryOp { op: _, rs } => {
                    if exp_type.is_none() {
                        exp_type = Some(check_or_find_type(rs.as_ref(), table, None))
                    } else {
                        let t = exp_type.as_ref().unwrap();

                        check_or_find_type(rs.as_ref(), table, Some(t.clone()));
                    }
                }

                Expr::BinOp { ls, op: _, rs } => {
                    if exp_type.is_none() {
                        let t = check_or_find_type(ls.as_ref(), table, None);

                        check_or_find_type(rs.as_ref(), table, Some(t.clone()));

                        exp_type = Some(t);
                    } else {
                        let t = exp_type.as_ref().unwrap();

                        // Left side is aleays checked first
                        check_or_find_type(rs.as_ref(), table, Some(t.clone()));
                        check_or_find_type(ls.as_ref(), table, Some(t.clone()));
                    }
                }
            }

            // There will never be a case where no type is
            // determined
            exp_type.unwrap()
        }

        for stmnt in &self.stmnts {
            match stmnt {
                Stmnt::Declare {
                    ident,
                    type_ident,
                    value,
                } => {
                    // Type is being inferred
                    if type_ident == "" {
                        table.push(Var::new(
                            ident.clone(),
                            check_or_find_type(value.as_ref(), &table, None),
                        ));
                    } else {
                        table.push(Var::new(
                            ident.clone(),
                            check_or_find_type(value.as_ref(), &table, Some(type_ident.clone())),
                        ));
                    }
                }
                Stmnt::Assign { ident, value } => {
                    // Find the type of the ident
                    for var in &table {
                        if &var.id == ident {
                            if check_or_find_type(value.as_ref(), &table, None) != var.t_id {
                                Block::throw_type_err(ident, &var.t_id);
                            }
                        }
                    }
                }
            }
        }

        if let Some(expr) = &self.expr {
            Some(check_or_find_type(&expr, &table, exp_type))
        } else {
            None
        }
    }

    fn throw_type_err(ident: &str, expct: &str) -> ! {
        println!(
            "{}: {} `{}` is not of type {}",
            "Error".red().bold(),
            "variable".bold(),
            ident,
            expct
        );

        std::process::exit(-1);
    }
}
