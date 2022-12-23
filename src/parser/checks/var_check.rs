use colored::*;

use crate::parser::ast::*;

impl Block {
    pub fn check_vars(&self, ext: &Vec<&String>) {
        // Bring all the variables defined in
        // the external scope into scope
        let mut table = ext.clone();

        fn check_expr(expr: &Expr, table: &Vec<&String>) {
            match expr {
                Expr::Ident(id) => {
                    if !table.contains(&id) {
                        Block::throw_undef_var_err(&id);
                    }
                }

                Expr::Block(block) => {
                    block.check_vars(&table);
                }

                Expr::UnaryOp { op: _, rs } => check_expr(rs.as_ref(), table),

                Expr::BinOp { ls, op: _, rs } => {
                    check_expr(ls.as_ref(), table);
                    check_expr(rs.as_ref(), table);
                }

                Expr::Const(_) => (),
            }
        }

        // Check each statment
        for stmnt in &self.stmnts {
            match stmnt {
                // Add a new variable to the table - shadowing is allowed
                Stmnt::Declare {
                    ident,
                    type_ident: _,
                    value,
                } => {
                    // Check before adding to prevent recursion
                    check_expr(value.as_ref(), &table);

                    table.push(ident);
                }

                Stmnt::Assign { ident, value } => {
                    if !table.contains(&ident) {
                        Self::throw_undef_var_err(&ident);
                    }

                    check_expr(value.as_ref(), &table);
                }
            }
        }

        // Check the ending expression
        if let Some(expr) = &self.expr {
            check_expr(&expr, &table);
        }
    }

    fn throw_undef_var_err(ident: &str) -> ! {
        println!(
            "{}: {} `{}` has not been {}",
            "Error".red().bold(),
            "variable".bold(),
            ident,
            "defined".bold()
        );

        std::process::exit(-1);
    }
}
