use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::object::Object;
use crate::source::SourceLocation;
use crate::token::Operator;

use std::collections::HashMap;
use string_interner::Sym;

#[derive(Default, Clone)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    bindings: HashMap<Sym, Object>,
}

pub struct Interpreter {
    // running state here
    environment: Environment,
}

impl Environment {
    fn new_inner(self) -> Environment {
        Environment {
            enclosing: Some(Box::new(self)),
            bindings: HashMap::default(),
        }
    }

    fn declare(&mut self, sym: Sym, obj: Object) -> Option<Object> {
        self.bindings.insert(sym, obj)
    }

    fn assign(&mut self, sym: Sym, obj: Object) -> Option<Object> {
        if self.bindings.contains_key(&sym) {
            self.bindings.insert(sym, obj)
        } else if let Some(enc) = &mut self.enclosing {
            enc.assign(sym, obj)
        } else {
            None
        }
    }

    fn is_defined(&self, sym: &Sym) -> bool {
        if self.bindings.contains_key(sym) {
            true
        } else if let Some(enc) = &self.enclosing {
            enc.is_defined(sym)
        } else {
            false
        }
    }

    fn get_by_sym(&self, sym: &Sym) -> Option<&Object> {
        let obj = self.bindings.get(sym);

        if obj.is_some() {
            obj
        } else if let Some(enc) = &self.enclosing {
            enc.get_by_sym(sym)
        } else {
            None
        }
    }

    /*
    fn get_by_token<'a>(&'a self, token: &crate::token::Token) -> Result<&'a Object, RuntimeError> {
        let sym = match token.kind {
            crate::token::TokenKind::Identifier(sym) => sym,
            _ => {
                return Err(RuntimeError::ice(
                    "Token not an identifier",
                    token.location.clone(),
                ))
            }
        };

        match self.bindings.get(&sym) {
            Some(obj) => Ok(obj),
            None => Err(RuntimeError::undefined_variable(token.location.clone())),
        }
    }
    */
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::default(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Option<Object>, RuntimeError> {
        let mut obj = None;
        for statement in statements {
            //~ eprintln!("{}", statement);
            use StmtKind::*;
            obj = match statement.kind {
                Expr(expr) => Some(self.evaluate(&expr)?),
                Print(expr) => {
                    println!("{}", self.evaluate(&expr)?);
                    None
                }
                VariableDeclaration(sym, maybe_init) => {
                    let obj = match maybe_init {
                        Some(init) => self.evaluate(&init)?,
                        None => Object::Nil,
                    };
                    self.environment.declare(sym, obj);
                    None
                }
                Block(stmts) => {
                    let environment = std::mem::take(&mut self.environment);
                    let (environment, err) = self.execute_block(stmts, environment.new_inner());
                    self.environment = environment;
                    if let Some(e) = err {
                        return Err(e);
                    }
                    None
                }
                If(cond, then_block, else_block) => {
                    if self.evaluate(&cond)?.is_truthy() {
                        self.interpret(vec![*then_block])?;
                    } else if let Some(e) = else_block {
                        self.interpret(vec![*e])?;
                    }
                    None
                }
            };
        }
        Ok(obj)
    }

    fn execute_block(
        &mut self,
        stmts: Vec<Stmt>,
        environment: Environment,
        //~ ) -> Result<(), RuntimeError> {
    ) -> (Environment, Option<RuntimeError>) {
        self.environment = environment;

        let result = self.interpret(stmts);

        let environment = std::mem::take(&mut self.environment);

        let env = match environment.enclosing {
            Some(e) => *e,
            None => panic!(),
        };

        let err = match result {
            Ok(_) => None,
            Err(e) => Some(e),
        };

        (env, err)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        use ExprKind::*;
        use Object::*;
        use Operator::*;
        Ok(match &expr.kind {
            Literal(literal) => literal.clone(),
            Unary(operator, operand_expr) => {
                let operand = self.evaluate(operand_expr)?;
                match operator {
                    Minus => {
                        let inner = match operand {
                            Number(n) => n,
                            _ => {
                                return Err(RuntimeError::type_error(
                                    "Attempt to arithmetically negate a non-number.",
                                    expr.location.clone(),
                                ))
                            }
                        };
                        Object::Number(-inner)
                    }
                    Bang => {
                        let inner = operand.is_truthy();
                        Object::Boolean(!inner)
                    }
                    _ => {
                        return Err(RuntimeError::ice(
                            "op is not a unary operator. bad syntax tree",
                            expr.location.clone(),
                        ))
                    }
                }
            }
            Binary(left_operand_expr, operator, right_operand_expr) => {
                let left = self.evaluate(left_operand_expr)?;
                let right = self.evaluate(right_operand_expr)?;
                match operator {
                    Minus => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n - right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot subtract, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot subtract, first arg not a number",
                                expr.location.clone(),
                            ))
                        }
                    },
                    Plus => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n + right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot add, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        (String(left_s), String(right_s)) => {
                            Object::String(left_s.concat(right_s))
                            //String(StringKind::Dynamic(
                            //left.string_concat(&right)
                            //.map_err(|_| Error::Runtime("Cannot concat, second arg not a string"))?
                            //))
                        }
                        (String(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot concat, second arg not a string",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot use plus, first arg not a number or string",
                                expr.location.clone(),
                            ))
                        }
                    },
                    Slash => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n / right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot divide, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot divide, first arg not a number",
                                expr.location.clone(),
                            ))
                        }
                    },
                    Star => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n * right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot multiply, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot multiply, first arg not a number",
                                expr.location.clone(),
                            ))
                        }
                    },
                    Greater => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n > right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            ))
                        }
                    },
                    Less => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n < right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            ))
                        }
                    },
                    GreaterEqual => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n >= right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            ))
                        }
                    },
                    LessEqual => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n <= right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            ))
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            ))
                        }
                    },
                    BangEqual => Object::Boolean(left != right),
                    EqualEqual => Object::Boolean(left == right),
                    Comma | Dot | Equal | Semicolon => {
                        return Err(RuntimeError::unimplemented(
                            "Unimplemented binary operator",
                            expr.location.clone(),
                        ))
                    }
                    Bang => {
                        return Err(RuntimeError::ice(
                            "op is not a binary operator. bad syntax tree",
                            expr.location.clone(),
                        ))
                    }
                }
            }
            Grouping(inside) => self.evaluate(inside)?,
            VariableAccess(sym) => self
                .environment
                .get_by_sym(sym)
                .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?
                .clone(),
            Assign(sym, expr) => {
                if self.environment.is_defined(sym) {
                    let value = self.evaluate(expr)?;

                    self.environment.assign(*sym, value.clone());

                    value
                } else {
                    return Err(RuntimeError::undefined_variable(expr.location.clone()));
                }
            }
            LogicalOr(left, right) => {
                let value = self.evaluate(left)?;

                if value.is_truthy() {
                    return Ok(value);
                }

                self.evaluate(right)?
            },
            LogicalAnd(left, right) => {
                let value = self.evaluate(left)?;

                if !value.is_truthy() {
                    return Ok(value);
                }

                self.evaluate(right)?
            },
        })
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    message: &'static str,
    kind: RuntimeErrorKind,
    location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    Ice,
    Unimplemented,
    TypeError,
    UndefinedVariable,
}

impl std::error::Error for RuntimeError {}

impl RuntimeError {
    pub fn ice(message: &'static str, location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message,
            location,
            kind: RuntimeErrorKind::Ice,
        }
    }

    pub fn unimplemented(message: &'static str, location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message,
            location,
            kind: RuntimeErrorKind::Unimplemented,
        }
    }

    pub fn type_error(message: &'static str, location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message,
            location,
            kind: RuntimeErrorKind::TypeError,
        }
    }

    pub fn undefined_variable(location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message: "Undefined variable",
            location,
            kind: RuntimeErrorKind::UndefinedVariable,
        }
    }
}

impl From<RuntimeError> for crate::result::Error {
    fn from(error: RuntimeError) -> Self {
        crate::result::Error::Runtime(error)
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Runtime Error {:?} at {}: {}",
            self.kind, self.location, self.message
        )
    }
}
