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

/*
use std::collections::HashSet;

trait EnvTrait {
    fn define(&mut self, sym: &Sym, value: Object);
    fn assign(&mut self, sym: &Sym, value: Object);
    fn get(&self, sym: &Sym) -> Option<Object>;
    fn is_defined(&self, sym: &Sym) -> bool;

}

impl EnvTrait for Environment {
    fn define(&mut self, sym: &Sym, value: Object) {
        self.define(sym, value);
    }

    fn assign(&mut self, sym: &Sym, value: Object) {
        self.assign(*sym, value);
    }

    fn get(&self, sym: &Sym) -> Option<Object> {
        self.get_by_sym(sym)
    }

    fn is_defined(&self, sym: &Sym) -> bool {
        self.is_defined(sym)
    }
}

impl EnvTrait for Closure {
    fn define(&mut self, _sym: &Sym, _value: Object) {
        panic!();
    }

    fn assign(&mut self, sym: &Sym, value: Object) {
        if self.bindings.contains(sym) {
            self.enclosing.borrow_mut().assign(sym, value)
        }
    }

    fn get(&self, sym: &Sym) -> Option<Object> {
        if self.bindings.contains(sym) {
            self.enclosing.borrow().get(sym)
        } else {
            None
        }
    }

    fn is_defined(&self, sym: &Sym) -> bool {
        self.bindings.contains(sym)
    }
}

impl EnvTrait for EnvOrClosure {
    fn define(&mut self, sym: &Sym, value: Object) {
        use EnvOrClosure::*;
        match self {
            Env(e) => EnvTrait::define(e, sym, value),
            Clos(e) => EnvTrait::define(e, sym, value),
        }
    }

    fn assign(&mut self, sym: &Sym, value: Object) {
        use EnvOrClosure::*;
        match self {
            Env(e) => EnvTrait::assign(e, sym, value),
            Clos(e) => EnvTrait::assign(e, sym, value),
        }
    }

    fn get(&self, sym: &Sym) -> Option<Object> {
        use EnvOrClosure::*;
        match self {
            Env(e) => EnvTrait::get(e, sym),
            Clos(e) => EnvTrait::get(e, sym),
        }
    }

    fn is_defined(&self, sym: &Sym) -> bool {
        use EnvOrClosure::*;
        match self {
            Env(e) => EnvTrait::is_defined(e, sym),
            Clos(e) => EnvTrait::is_defined(e, sym),
        }
    }
}

pub enum EnvOrClosure {
    Env(Environment),
    Clos(Closure),
}

pub struct Closure {
    enclosing: Rc<RefCell<EnvOrClosure>>,
    bindings: HashSet<Sym>,
}
*/

#[derive(Debug, Default)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<Sym, Object>,
}

#[derive(Default)]
pub struct Interpreter {
    compatibility_mode: bool,
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
        //~ if self.bindings.contains_key(&sym) {
        //~     self.bindings.insert(sym, obj)
        if let std::collections::hash_map::Entry::Occupied(mut e) = self.bindings.entry(sym) {
            Some(e.insert(obj))
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
    pub fn new(compatibility_mode: bool) -> Interpreter {
        Interpreter {
            compatibility_mode,
            ..Interpreter::default()
        }
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
        fn clock(
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

        fn read(
            _interpreter: &mut Interpreter,
            _args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            use std::io::{self, BufRead};

            let mut line = String::new();
            let stdin = io::stdin();
            stdin.lock().read_line(&mut line).unwrap();
            let line = line.trim().to_string();

            Ok(Object::String(crate::object::StringKind::Dynamic(line)))
        }

        fn to_string(
            _interpreter: &mut Interpreter,
            args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            let obj = &args[0];

            let s = format!("{}", obj);

            Ok(Object::dynamic_string(s))
        }

        fn to_number(
            _interpreter: &mut Interpreter,
            args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            let obj = &args[0];

            Ok(match obj {
                Object::Number(_) => obj.clone(),
                Object::String(s) => match s.to_string().parse::<f64>() {
                    Ok(n) => Object::Number(n),
                    Err(_) => Object::Nil,
                },
                _ => Object::Nil,
            })
        }

        fn getc(
            _interpreter: &mut Interpreter,
            _args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            use std::io::Read;
            match std::io::stdin().bytes().next() {
                Some(r) => match r {
                    Ok(n) => Ok(Object::Number(n as f64)),
                    Err(_e) => Ok(Object::Number(-1.0)),
                },
                None => Ok(Object::Number(-1.0)),
            }
        }

        fn chr(_interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, ErrorOrReturn> {
            let obj = &args[0];

            Ok(match obj {
                Object::Number(n) => {
                    Object::dynamic_string(std::str::from_utf8(&[*n as u8]).unwrap().to_string())
                }
                _ => Object::Nil,
            })
        }

        fn exit(
            _interpreter: &mut Interpreter,
            args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            let obj = &args[0];

            match obj {
                Object::Number(n) => std::process::exit(*n as i32),
                _ => std::process::exit(0),
            }
        }

        fn print_error(
            _interpreter: &mut Interpreter,
            args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            let message = &args[0];

            eprintln!("{}", message);

            Ok(Object::Nil)
        }

        self.define_global_item("clock", 0, clock);
        self.define_global_item("read", 0, read);
        self.define_global_item("to_string", 1, to_string);
        self.define_global_item("to_number", 1, to_number);
        self.define_global_item("getc", 0, getc);
        self.define_global_item("chr", 1, chr);
        self.define_global_item("exit", 1, exit);
        self.define_global_item("print_error", 1, print_error);

        /*
        fn test(
            _interpreter: &mut Interpreter,
            _args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            let s = "string";

            let b = Object::dynamic_string(s.to_string()) == Object::static_string_from_str(s);

            Ok(Object::Boolean(b))
        }

        self.define_global_item("test", 0, test);
        */

        fn require(
            _interpreter: &mut Interpreter,
            args: Vec<Object>,
        ) -> Result<Object, ErrorOrReturn> {
            let filename_obj = &args[0];

            match filename_obj {
                Object::String(s) => {
                    let filename = s.to_string();

                    let source = std::fs::read_to_string(filename).unwrap();

                    let result = crate::run_string(source.clone(), 0, false);

                    match result {
                        Ok(obj) => Ok(obj.unwrap()),
                        Err(e) => {
                            eprintln!(
                                "error on line {:?}",
                                crate::source::SourceLocation::error_line_number(&e, &source)
                            );
                            panic!()
                        }
                    }
                }
                _ => Ok(Object::Nil),
            }
        }

        self.define_global_item("require", 1, require);
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

    pub fn create_closure(&self) -> Rc<RefCell<Environment>> {
        if self.compatibility_mode {
            Environment::new_inner(self.environment.clone())
        } else {
            self.environment.borrow().flat_copy()
        }
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
            Break(expr) => {
                return Err(ErrorOrReturn::Break(match expr {
                    Some(e) => Some(self.evaluate(e)?),
                    None => None,
                }));
            }
            Class(name, superclass_name, method_decls, associated_decls) => {
                let method_environment = self.create_closure();

                let superclass = if let Some(superclass_name) = superclass_name {
                    let superclass = self
                        .get_binding(superclass_name)
                        .ok_or(RuntimeError::undefined_variable(SourceLocation::bullshit()))?;

                    let super_name = {
                        let mut interner = INTERNER.write().unwrap();
                        interner.get_or_intern("super")
                    };

                    method_environment
                        .borrow_mut()
                        .define(&super_name, superclass.clone());

                    if let Object::Callable(Callable::Class(class)) = superclass {
                        Some(class)
                    } else {
                        return Err(RuntimeError::type_error(
                            "A class can only inherit from a class",
                            SourceLocation::bullshit(),
                        )
                        .into());
                    }
                } else {
                    None
                };

                let mut methods = HashMap::default();

                for stmt in method_decls {
                    let (name, func) = match &stmt.kind {
                        StmtKind::FunctionDeclaration(name, func) => (name, func),
                        _ => {
                            return Err(RuntimeError::ice(
                                "statement in class is not a method or associated function. bad syntax tree",
                                SourceLocation::bullshit(),
                            )
                            .into());
                        }
                    };

                    let mut new_func = func.clone();

                    new_func.closure = method_environment.clone();

                    methods.insert(*name, new_func);
                }

                let mut associated_functions = HashMap::default();

                for stmt in associated_decls {
                    let (name, func) = match &stmt.kind {
                        StmtKind::FunctionDeclaration(name, func) => (name, func),
                        _ => {
                            return Err(RuntimeError::ice(
                                "statement in class is not a method or associated function. bad syntax tree",
                                SourceLocation::bullshit(),
                            )
                            .into());
                        }
                    };

                    let mut new_func = func.clone();

                    new_func.closure = method_environment.clone();

                    associated_functions.insert(*name, new_func);
                }

                let obj = Object::Callable(Callable::Class(Rc::new(crate::object::Class {
                    name: *name,
                    superclass,
                    methods,
                    associated_functions,
                })));

                method_environment.borrow_mut().define(name, obj.clone());

                self.environment.borrow_mut().define(name, obj);

                Object::Nil
            }
            Expr(expr) => self.evaluate(expr)?,
            FunctionDeclaration(name, func) => {
                let closure = self.create_closure();

                let mut new_func = func.clone();

                new_func.closure = closure.clone();

                let obj = Object::Callable(Callable::Lox(new_func));
                closure.borrow_mut().define(name, obj.clone());
                self.environment.borrow_mut().define(name, obj);
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
                    match self.interpret_statement(body) {
                        Ok(_) => (),
                        Err(e) => match e {
                            ErrorOrReturn::RuntimeError(_) => return Err(e),
                            ErrorOrReturn::Return(_) => return Err(e),
                            ErrorOrReturn::Break(_v) => break,
                        },
                    }
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

    pub fn call_object(
        &mut self,
        callee: &Object,
        arguments: Vec<Object>,
        location: SourceLocation,
    ) -> Result<Object, ErrorOrReturn> {
        match callee {
            Object::Callable(c) => self.call(c, arguments)?,
            _ => {
                return Err(
                    RuntimeError::type_error("Attempt to call uncallable type", location).into(),
                );
            }
        };
        todo!()
    }

    pub fn call(
        &mut self,
        callee: &Callable,
        arguments: Vec<Object>,
    ) -> Result<Object, ErrorOrReturn> {
        use Callable::*;
        match callee {
            Native(f) => (f.func)(self, arguments),
            Lox(f) => {
                let closure = &f.closure;

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
                        ErrorOrReturn::Break(_v) => todo!(),
                    },
                })
            }
            Class(class) => {
                let instance = Object::new_instance(class.clone());

                let (init, this) = {
                    let mut interner = INTERNER.write().unwrap();
                    let init = interner.get_or_intern("init");
                    let this = interner.get_or_intern("this");
                    (init, this)
                };

                if let Some(mut method) = class.get_method(&init) {
                    let new_env = Environment::new_inner(method.closure.clone());

                    new_env.borrow_mut().define(&this, instance.clone());

                    method.closure = new_env;

                    let callable = crate::object::Callable::Lox(method);

                    self.call(&callable, arguments)?;
                }

                Ok(instance)
            }
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Object, ErrorOrReturn> {
        use ExprKind::*;
        use Object::*;
        use Operator::*;
        Ok(match &expr.kind {
            Literal(literal) => match literal {
                Callable(crate::object::Callable::Lox(func)) => {
                    let closure = self.create_closure();

                    let mut new_func = func.clone();

                    new_func.closure = closure;

                    Object::Callable(crate::object::Callable::Lox(new_func))
                }
                _ => literal.clone(),
            },
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
                    Percent => match (left, right) {
                        (Number(left_n), Number(right_n)) => Object::Number(left_n % right_n),
                        (Number(_), _) => {
                            return Err(RuntimeError::type_error(
                                "Cannot modulo, second arg not a number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                        _ => {
                            return Err(RuntimeError::type_error(
                                "Cannot modulo, first arg not a number",
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
                    Comma | Dot | Equal | Semicolon | Bang | MinusEqual | PlusEqual
                    | SlashEqual | StarEqual | PercentEqual => {
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
            PropertyAccess(obj_expr, name) => {
                let location = obj_expr.location.clone();

                let obj = self.evaluate(obj_expr)?;

                match obj {
                    Instance(instance) => match instance.borrow().get(name) {
                        Some(o) => o,
                        None => match instance.borrow().class.get_method(name) {
                            Some(mut method) => {
                                let new_env = Environment::new_inner(method.closure.clone());

                                let mut interner = INTERNER.write().unwrap();
                                let sym = interner.get_or_intern("this");

                                new_env
                                    .borrow_mut()
                                    .define(&sym, Object::Instance(instance.clone()));

                                method.closure = new_env;

                                Object::Callable(crate::object::Callable::Lox(method))
                            }
                            None => {
                                return Err(RuntimeError::type_error(
                                    "Cannot access nonexistent property or method",
                                    location,
                                )
                                .into())
                            }
                        },
                    },
                    Callable(crate::object::Callable::Class(class)) => {
                        match class.get_function(name) {
                            Some(f) => Object::Callable(crate::object::Callable::Lox(f)),
                            None => {
                                return Err(RuntimeError::type_error(
                                    "Cannot access nonexistent associated function",
                                    location,
                                )
                                .into())
                            }
                        }
                    }
                    _ => {
                        return Err(RuntimeError::type_error(
                            "Cannot access property, not an instance or class",
                            location,
                        )
                        .into())
                    }
                }
            }
            PropertyAssign(obj, name, value) => {
                let location = obj.location.clone();

                let obj = self.evaluate(obj)?;

                match obj {
                    Instance(instance) => {
                        let value = self.evaluate(value)?;

                        instance.borrow_mut().set(name, value.clone());

                        value
                    }
                    _ => {
                        return Err(RuntimeError::type_error(
                            "Cannot access property, not an instance",
                            location,
                        )
                        .into())
                    }
                }
            }
            This => {
                let mut interner = INTERNER.write().unwrap();
                let sym = interner.get_or_intern("this");

                self.get_binding(&sym)
                    .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?
            }
            Super(method_name) => {
                let (super_name, this_name) = {
                    let mut interner = INTERNER.write().unwrap();
                    (
                        interner.get_or_intern("super"),
                        interner.get_or_intern("this"),
                    )
                };

                let class = self
                    .get_binding(&super_name)
                    .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?;

                let this = self
                    .get_binding(&this_name)
                    .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?;

                let mut method =
                    if let Object::Callable(crate::object::Callable::Class(class)) = class {
                        class
                            .get_method(method_name)
                            .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?
                    } else {
                        return Err(RuntimeError::ice(
                            "super is not a class. bad syntax tree",
                            SourceLocation::bullshit(),
                        )
                        .into());
                    };

                let new_env = Environment::new_inner(method.closure.clone());

                new_env.borrow_mut().define(&this_name, this);

                method.closure = new_env;

                Object::Callable(crate::object::Callable::Lox(method))
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    message: &'static str,
    kind: RuntimeErrorKind,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    Ice,
    Unimplemented,
    TypeError,
    UndefinedVariable,
}

pub enum ErrorOrReturn {
    RuntimeError(RuntimeError),
    Return(Object),
    Break(Option<Object>),
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
