use crate::ast::Expr;
use crate::object::Object;
use crate::parser::Parser;
use crate::result::{Result, Error};
use crate::scanner::Scanner;
use crate::store::SourceStore;
use crate::store::Store;

pub struct Interpreter {
    // running state here
}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn evaluate<'a>(&mut self, expr: &'a Expr) -> Result<Object> {
        use Expr::*;
        use Object::*;
        use crate::scanner::Operator::*;
        Ok(match *expr {
            Literal(ref literal) => literal.clone(),
            Unary(ref operator, ref operand_expr) => {
                let operand = self.evaluate(operand_expr.as_ref())?;
                match operator {
                    Minus => {
                        let inner = match operand {
                            Number(n) => n,
                            _ => return Err(Error::Runtime("Attempt to arithmetically negate a non-number.")),
                        };
                        Object::Number(-inner)
                    },
                    Bang => {
                        let inner = operand.is_truthy();
                        Object::Boolean(!inner)
                    },
                    _ => return Err(Error::Ice("op is not a unary operator. bad syntax tree")),
                }
            },
            Binary(ref left_operand_expr, ref operator, ref right_operand_expr) => {
                let left = self.evaluate(left_operand_expr.as_ref())?;
                let right = self.evaluate(right_operand_expr.as_ref())?;
                match operator {
                    Minus => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Number(left_n - right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot subtract, second arg not a number")),
                            _ => return Err(Error::Runtime("Cannot subtract, first arg not a number")),
                        }
                    },
                    Plus => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Number(left_n + right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot add, second arg not a number")),
                            (StaticString(_), _) => return Err(Error::Unimplemented("Concat not implemented")),
                            _ => return Err(Error::Runtime("Cannot use plus, first arg not a number or string")),
                        }
                    },
                    Slash => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Number(left_n / right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot divide, second arg not a number")),
                            _ => return Err(Error::Runtime("Cannot divide, first arg not a number")),
                        }
                    },
                    Star => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Number(left_n * right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot multiply, second arg not a number")),
                            _ => return Err(Error::Runtime("Cannot multiply, first arg not a number")),
                        }
                    },
                    Greater => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Boolean(left_n > right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot compare, second arg not a number")),
                            _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                        }
                    },
                    Less => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Boolean(left_n < right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot compare, second arg not a number")),
                            _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                        }
                    },
                    GreaterEqual => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Boolean(left_n >= right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot compare, second arg not a number")),
                            _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                        }
                    },
                    LessEqual => {
                        match (left, right) {
                            (Number(left_n), Number(right_n)) => Object::Boolean(left_n <= right_n),
                            (Number(_), _) => return Err(Error::Runtime("Cannot compare, second arg not a number")),
                            _ => return Err(Error::Runtime("Cannot compare, first arg not a number")),
                        }
                    },
                    BangEqual => Object::Boolean(left != right),
                    EqualEqual => Object::Boolean(left == right),
                    //Comma,
                    //Dot,
                    //Equal,
                    _ => return Err(Error::Unimplemented("Unimplemented binary operator")),
                }
            },
            Grouping(ref inside) => self.evaluate(inside.as_ref().clone())?,
        })
    }
}

pub fn run_string(source: String) -> Result<String> {
    let mut source_store = SourceStore::new();

    // move the source in, no need to allocate again
    let id = source_store.add_from_source(source);

    source_store.set_eof(id);

    //let mut id_store = Store::new();
    //let mut string_store = Store::new();

    let mut static_store = Store::new();

    let mut sc = Scanner::new(id);
    let tokens = sc.collect_or_first_error(&source_store, &mut static_store)?;

    let mut parser = Parser::new(tokens);
    let expr = parser.parse()?;
    //println!("{:?}", expr);

    let mut interpreter = Interpreter::new();

    let obj = interpreter.evaluate(&expr)?;

    Ok(format!("{}", obj))
}
