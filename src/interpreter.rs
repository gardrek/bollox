use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::object::Callable;
use crate::object::NativeFunction;
use crate::object::Object;
use crate::source::SourceLocation;
use crate::token::Operator;
use crate::INTERNER;

use string_interner::Sym;

#[derive(Debug, Default, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<Sym, Object>,
}

#[derive(Default)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
}

impl Environment {
    pub fn new_inner(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            enclosing: Some(env),
            bindings: HashMap::default(),
        }))
    }

    pub fn define(&mut self, sym: &Sym, obj: Object) -> Option<Object> {
        self.bindings.insert(*sym, obj)
    }

    fn assign(&mut self, sym: Sym, obj: Object) -> Option<Object> {
        if self.bindings.contains_key(&sym) {
            self.bindings.insert(sym, obj)
        } else if let Some(enc) = &mut self.enclosing {
            enc.borrow_mut().assign(sym, obj)
        } else {
            None
        }
    }

    fn is_defined(&self, sym: &Sym) -> bool {
        if self.bindings.contains_key(sym) {
            true
        } else if let Some(enc) = &self.enclosing {
            enc.borrow().is_defined(sym)
        } else {
            false
        }
    }

    fn get_by_sym(&self, sym: &Sym) -> Option<Object> {
        let obj = self.bindings.get(sym);

        if obj.is_some() {
            obj.cloned()
        } else if let Some(enc) = &self.enclosing {
            enc.borrow().get_by_sym(sym)
        } else {
            None
        }
    }

    pub fn flat_copy(&self) -> Rc<RefCell<Environment>> {
        let bindings = self.bindings_flattened();

        Rc::new(RefCell::new(Environment {
            enclosing: None,
            bindings,
        }))
    }

    pub fn bindings_flattened(&self) -> HashMap<Sym, Object> {
        let bindings = HashMap::new();
        self.flatten_recurse(bindings)
    }

    fn flatten_recurse(&self, bindings: HashMap<Sym, Object>) -> HashMap<Sym, Object> {
        //~ bindings.extend(self.bindings.clone());

        let mut next_bindings = self.bindings.clone();

        next_bindings.extend(bindings);

        match &self.enclosing {
            Some(e) => e.borrow().flatten_recurse(next_bindings),
            None => next_bindings,
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter::default()
    }

    fn get_binding(&self, sym: &Sym) -> Option<Object> {
        let env = self.globals.borrow();
        let obj = env.bindings.get(sym);

        if obj.is_some() {
            obj.cloned()
        } else {
            let env = self.environment.borrow();
            let obj = env.bindings.get(sym);

            if obj.is_some() {
                obj.cloned()
            } else if let Some(enc) = &self.environment.borrow().enclosing {
                enc.borrow().get_by_sym(sym)
            } else {
                None
            }
        }
    }

    pub fn init_global_environment(&mut self) {
        fn clock_fn(
            _interpreter: &mut Interpreter,
            _args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            use std::time::{SystemTime, UNIX_EPOCH};

            Ok(Object::Number(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards")
                    .as_millis() as f64
                    / 1000.0,
            ))
        }

        self.define_global_item("clock", 0, clock_fn);
    }

    fn define_global_item(
        &mut self,
        name: &'static str,
        arity: usize,
        func: fn(&mut Interpreter, Vec<Object>) -> Result<Object, ErrorOrReturn>,
    ) {
        let mut interner = INTERNER.write().unwrap();

        let sym = interner.get_or_intern(name);

        self.globals.borrow_mut().define(
            &sym,
            Object::Callable(Callable::Native(NativeFunction {
                name: "clock",
                arity,
                func,
            })),
        );
    }

    pub fn interpret_statement(&mut self, statement: &Stmt) -> Result<Object, ErrorOrReturn> {
        use StmtKind::*;
        Ok(match &statement.kind {
            Block(stmts) => {
                let environment = std::mem::take(&mut self.environment);
                let (environment, err) =
                    self.execute_block(stmts, Environment::new_inner(environment));
                self.environment = environment;
                if let Some(e) = err {
                    return Err(e);
                }
                Object::Nil
            }
            Expr(expr) => self.evaluate(expr)?,
            FunctionDeclaration(func) => {
                let name = func.name;
                let closure = self.environment.borrow().flat_copy();
                let obj = Object::Callable(Callable::Lox(func.clone(), closure.clone()));
                closure.borrow_mut().define(&name, obj.clone());
                self.environment.borrow_mut().define(&name, obj);
                Object::Nil
            }
            If(cond, then_block, else_block) => {
                if self.evaluate(cond)?.is_truthy() {
                    self.interpret_statement(then_block)?;
                } else if let Some(e) = else_block {
                    self.interpret_statement(e)?;
                }
                Object::Nil
            }
            Print(expr) => {
                println!("{}", self.evaluate(expr)?);
                Object::Nil
            }
            Return(expr) => {
                return Err(ErrorOrReturn::Return(self.evaluate(expr)?));
            }
            VariableDeclaration(sym, maybe_init) => {
                let obj = match maybe_init {
                    Some(init) => self.evaluate(init)?,
                    None => Object::Nil,
                };
                self.environment.borrow_mut().define(sym, obj);
                Object::Nil
            }
            While(cond, body) => {
                while self.evaluate(cond)?.is_truthy() {
                    self.interpret_statement(body)?;
                }
                Object::Nil
            }
        })
    }

    pub fn interpret_slice(&mut self, statements: &[Stmt]) -> Result<Object, ErrorOrReturn> {
        let mut obj = None;
        for statement in statements {
            //~ eprintln!("{}", statement);
            obj = Some(self.interpret_statement(statement)?);
        }

        Ok(match obj {
            Some(o) => o,
            None => Object::Nil,
        })
    }

    fn execute_block(
        &mut self,
        stmts: &[Stmt],
        environment: Rc<RefCell<Environment>>,
    ) -> (Rc<RefCell<Environment>>, Option<ErrorOrReturn>) {
        self.environment = environment;

        let result = self.interpret_slice(stmts);

        let environment = std::mem::take(&mut self.environment);

        let env = match &environment.borrow_mut().enclosing {
            Some(e) => e.clone(),
            None => panic!(),
        };

        let err = match result {
            Ok(_) => None,
            Err(e) => Some(e),
        };

        (env, err)
    }

    pub fn call(
        &mut self,
        callee: &Callable,
        arguments: Vec<Object>,
    ) -> Result<Object, ErrorOrReturn> {
        use Callable::*;
        match callee {
            Native(f) => (f.func)(self, arguments),
            Lox(f, closure) => {
                let call_environment = Environment::new_inner(closure.clone());

                for (i, arg) in arguments.into_iter().enumerate() {
                    call_environment.borrow_mut().define(&f.parameters[i], arg);
                }

                let old_environment = std::mem::take(&mut self.environment);

                self.environment = call_environment;

                let ret = self.interpret_slice(&f.body[..]);

                self.environment = old_environment;

                Ok(match ret {
                    Ok(obj) => obj,
                    Err(eor) => match eor {
                        ErrorOrReturn::RuntimeError(e) => return Err(e.into()),
                        ErrorOrReturn::Return(v) => v,
                    },
                })
            }
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Object, ErrorOrReturn> {
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
                                )
                                .into());
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
                        )
                        .into())
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
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot subtract, first arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Plus => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n + right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot add, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
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
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot use plus, first arg not a number or string",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Slash => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n / right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot divide, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot divide, first arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Star => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n * right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot multiply, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot multiply, first arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Greater => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n > right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Less => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n < right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    GreaterEqual => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n >= right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    LessEqual => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Boolean(left_n <= right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot compare, first arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    BangEqual => Object::Boolean(left != right),
                    EqualEqual => Object::Boolean(left == right),
                    Comma | Dot | Equal | Semicolon => {
                        return Err(RuntimeError::unimplemented(
                            "Unimplemented binary operator",
                            expr.location.clone(),
                        )
                        .into());
                    }
                    Bang => {
                        return Err(RuntimeError::ice(
                            "op is not a binary operator. bad syntax tree",
                            expr.location.clone(),
                        )
                        .into());
                    }
                }
            }
            Grouping(inside) => self.evaluate(inside)?,
            VariableAccess(sym) => self
                .get_binding(sym)
                .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?,
            Assign(sym, expr) => {
                if self.environment.borrow().is_defined(sym) {
                    let value = self.evaluate(expr)?;

                    self.environment.borrow_mut().assign(*sym, value.clone());

                    value
                } else {
                    return Err(RuntimeError::undefined_variable(expr.location.clone()).into());
                }
            }
            LogicalOr(left, right) => {
                let value = self.evaluate(left)?;

                if value.is_truthy() {
                    return Ok(value);
                }

                self.evaluate(right)?
            }
            LogicalAnd(left, right) => {
                let value = self.evaluate(left)?;

                if !value.is_truthy() {
                    return Ok(value);
                }

                self.evaluate(right)?
            }
            Call(callee, args) => {
                let location = callee.location.clone();

                let callee = self.evaluate(callee)?;

                let mut evaluated_args = vec![];

                for a in args {
                    evaluated_args.push(self.evaluate(a)?);
                }

                let callee = match callee {
                    Callable(c) => c,
                    _ => {
                        return Err(RuntimeError::type_error(
                            "Attempt to call uncallable type",
                            location,
                        )
                        .into());
                    }
                };

                if callee.arity() != evaluated_args.len() {
                    return Err(RuntimeError::type_error(
                        "Incorrect number of arguments",
                        location,
                    )
                    .into());
                }

                self.call(&callee, evaluated_args)?
            }
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

#[derive(Debug, Clone)]
pub enum ErrorOrReturn {
    RuntimeError(RuntimeError),
    Return(Object),
}

impl From<RuntimeError> for ErrorOrReturn {
    fn from(other: RuntimeError) -> Self {
        ErrorOrReturn::RuntimeError(other)
    }
}

impl std::error::Error for RuntimeError {}

//~ impl std::error::Error for Exception {}

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
