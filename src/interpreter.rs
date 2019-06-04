use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::object::Object;
use crate::result::{Error, Result};

pub struct Interpreter {
    // running state here
}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Option<Object>> {
        let mut obj = None;
        for statement in statements {
            eprintln!("{}", statement);
            use StmtKind::*;
            obj = match statement.kind {
                Expr(expr) => Some(self.evaluate(&expr)?),
                Print(expr) => {
                    println!("{}", self.evaluate(&expr)?);
                    None
                }
                VariableDeclaration(_, _) => unimplemented!(),
            };
        }
        Ok(obj)
    }

    fn evaluate<'a>(&mut self, expr: &'a Expr) -> Result<Object> {
        use crate::scanner::Operator::*;
        use ExprKind::*;
        use Object::*;
        Ok(match expr.kind {
            Literal(ref literal) => literal.clone(),
            Unary(ref operator, ref operand_expr) => {
                let operand = self.evaluate(operand_expr.as_ref())?;
                match operator {
                    Minus => {
                        let inner = match operand {
                            Number(n) => n,
                            _ => {
                                return Err(Error::Runtime(
                                    "Attempt to arithmetically negate a non-number.",
                                ))
                            }
                        };
                        Object::Number(-inner)
                    }
                    Bang => {
                        let inner = operand.is_truthy();
                        Object::Boolean(!inner)
                    }
                    _ => return Err(Error::Ice("op is not a unary operator. bad syntax tree")),
                }
            }
            Binary(ref left_operand_expr, ref operator, ref right_operand_expr) => {
                let left = self.evaluate(left_operand_expr.as_ref())?;
                let right = self.evaluate(right_operand_expr.as_ref())?;
                match operator {
                    Minus => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n - right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot subtract, second arg not a number"))
                        }
                        _ => return Err(Error::Runtime("Cannot subtract, first arg not a number")),
                    },
                    Plus => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n + right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot add, second arg not a number"))
                        }
                        (String(left_s), String(right_s)) => {
                            Object::String(left_s.concat(right_s))
                            //String(StringKind::Dynamic(
                            //left.string_concat(&right)
                            //.map_err(|_| Error::Runtime("Cannot concat, second arg not a string"))?
                            //))
                        }
                        (String(_), _) => {
                            return Err(Error::Runtime("Cannot concat, second arg not a string"))
                        }
                        _ => {
                            return Err(Error::Runtime(
                                "Cannot use plus, first arg not a number or string",
                            ))
                        }
                    },
                    Slash => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n / right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot divide, second arg not a number"))
                        }
                        _ => return Err(Error::Runtime("Cannot divide, first arg not a number")),
                    },
                    Star => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n * right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot multiply, second arg not a number"))
                        }
                        _ => return Err(Error::Runtime("Cannot multiply, first arg not a number")),
                    },
                    Greater => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n > right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot compare, second arg not a number"))
                        }
                        _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                    },
                    Less => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n < right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot compare, second arg not a number"))
                        }
                        _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                    },
                    GreaterEqual => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n >= right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot compare, second arg not a number"))
                        }
                        _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                    },
                    LessEqual => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n <= right_n),
                        (Number(_), _) => {
                            return Err(Error::Runtime("Cannot compare, second arg not a number"))
                        }
                        _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                    },
                    BangEqual => Object::Boolean(left != right),
                    EqualEqual => Object::Boolean(left == right),
                    //Comma,
                    //Dot,
                    //Equal,
                    _ => return Err(Error::Unimplemented("Unimplemented binary operator")),
                }
            }
            Grouping(ref inside) => self.evaluate(inside.as_ref().clone())?,
            VariableAccess(_) => unimplemented!(),
        })
    }
}
