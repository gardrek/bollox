use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::interpreter::ControlFlow;
use crate::interpreter::Environment;
use crate::interpreter::Interpreter;
use crate::token::{ReservedWord, Token, TokenKind};
use crate::INTERNER;

use string_interner::Sym;

pub fn sym_to_str(sym: &Sym) -> String {
    let interner = INTERNER.read().unwrap();
    interner.resolve(*sym).unwrap().into()
}

#[derive(Clone)]
pub enum Object {
    Nil,
    Boolean(bool),
    Number(f64),
    String(StringKind),
    NativeFunc(NativeFunction),
    LoxFunc(LoxFunction),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    Array(Rc<RefCell<Vec<Object>>>),
}

impl Object {
    pub fn nil() -> Object {
        Object::Nil
    }

    pub fn boolean(b: bool) -> Object {
        Object::Boolean(b)
    }

    pub fn number(n: f64) -> Object {
        Object::Number(n)
    }

    pub fn static_string(s: Sym) -> Object {
        Object::String(StringKind::Static(s))
    }

    pub fn dynamic_string(s: String) -> Object {
        Object::String(StringKind::Dynamic(s))
    }

    pub fn new_instance(class: Rc<Class>) -> Object {
        Object::Instance(Rc::new(RefCell::new(Instance {
            class,
            fields: HashMap::default(),
        })))
    }

    pub fn arity(&self) -> Option<usize> {
        use Object::*;
        Some(match self {
            NativeFunc(f) => f.arity,
            LoxFunc(f) => f.parameters.len(),
            Class(class) => {
                let sym = {
                    let mut interner = INTERNER.write().unwrap();
                    interner.get_or_intern("init")
                };

                if let Some(f) = class.get_method(&sym) {
                    f.parameters.len()
                } else {
                    0
                }
            }
            _ => return None,
        })
    }
}

#[derive(Debug, Clone)]
pub enum StringKind {
    Dynamic(String),
    Static(Sym),
    Cat(Box<StringKind>, Box<StringKind>),
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Sym,
    pub superclass: Option<Rc<Class>>,
    pub methods: HashMap<Sym, LoxFunction>,
    pub associated_functions: HashMap<Sym, LoxFunction>,
}

impl Class {
    pub fn get_method(&self, name: &Sym) -> Option<LoxFunction> {
        match self.methods.get(name) {
            Some(method) => {
                let method = method.clone();
                Some(method)
            }
            None => {
                if let Some(superclass) = &self.superclass {
                    superclass.get_method(name)
                } else {
                    None
                }
            }
        }
    }

    pub fn get_function(&self, name: &Sym) -> Option<LoxFunction> {
        match self.associated_functions.get(name) {
            Some(f) => {
                let f = f.clone();
                Some(f)
            }
            None => {
                if let Some(superclass) = &self.superclass {
                    superclass.get_function(name)
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Rc<Class>,
    fields: HashMap<Sym, Object>,
}

impl Instance {
    pub fn get(&self, name: &Sym) -> Option<Object> {
        self.fields.get(name).cloned()
    }

    pub fn set(&mut self, name: &Sym, value: Object) {
        self.fields.insert(*name, value);
    }
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    pub parameters: Vec<Sym>,
    pub body: Vec<crate::ast::Stmt>,
    pub closure: Rc<RefCell<Environment>>,
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: &'static str,
    pub arity: usize,
    pub func: fn(&mut Interpreter, Vec<Object>) -> Result<Object, ControlFlow>,
    pub closure: Option<Rc<RefCell<Environment>>>,
}

impl StringKind {
    pub fn concat(self, other: Self) -> Self {
        use StringKind::*;
        match (&self, &other) {
            (Dynamic(_), _) | (_, Dynamic(_)) => Dynamic(format!("{}{}", self, other)),
            (_, _) => Cat(Box::new(self), Box::new(other)),
        }
    }
}

impl Object {
    pub fn from_token(token: &Token) -> Option<Object> {
        Some(match &token.kind {
            TokenKind::Number(value) => Object::number(*value),
            TokenKind::StaticString(sym) => Object::static_string(*sym),
            TokenKind::Reserved(word) => match word {
                ReservedWord::True => Object::boolean(true),
                ReservedWord::False => Object::boolean(false),
                ReservedWord::Nil => Object::nil(),
                _ => return None,
            },
            _ => return None,
        })
    }

    // TODO: this will be the version of the function if and when we switch the parser to
    // using the scanner directly instead of a vec<Token>
    pub fn _from_owned_token(token: Token) -> Option<Object> {
        use Object::*;
        Some(match &token.kind {
            TokenKind::Number(value) => Number(*value),
            TokenKind::StaticString(sym) => String(StringKind::Static(*sym)),
            TokenKind::Reserved(word) => match word {
                ReservedWord::True => Boolean(true),
                ReservedWord::False => Boolean(false),
                ReservedWord::Nil => Nil,
                _ => return None,
            },
            _ => return None,
        })
    }

    pub fn is_truthy(&self) -> bool {
        use Object::*;
        match self {
            Nil => false,
            Boolean(b) => *b,
            _ => true,
        }
    }

    /*
    pub fn as_number(&self) -> Option<f64> {
        use Object::*;
        Some(match self {
            Number(n) => *n,
            _ => return None,
        })
    }

    pub fn unwrap_as_string(&self) -> String {
        if let Object::String(kind) = self {
            kind.to_string()
        } else {
            panic!("unwrap_as_string called on non-string object")
        }
    }
    */
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        // here's a hacky way to not have to add another Rc
        Rc::ptr_eq(&self.closure, &other.closure)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        use Object::*;
        use StringKind::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Boolean(a), Boolean(b)) => a == b,
            (Number(a), Number(b)) => a == b,
            (String(a_kind), String(b_kind)) => match (a_kind, b_kind) {
                (Static(a), Static(b)) => a == b,
                (Dynamic(_), Dynamic(_))
                | (Dynamic(_), _)
                | (_, Dynamic(_))
                | (Cat(_, _), Cat(_, _))
                | (Cat(_, _), _)
                | (_, Cat(_, _)) => a_kind.to_string() == b_kind.to_string(),
            },
            (NativeFunc(_a), NativeFunc(_b)) => false, // TODO: try to improve this?
            (LoxFunc(a), LoxFunc(b)) => a == b,
            (Class(rc_a), Class(rc_b)) => Rc::ptr_eq(rc_a, rc_b),
            (Instance(rc_a), Instance(rc_b)) => Rc::ptr_eq(rc_a, rc_b),
            (Array(rc_a), Array(rc_b)) => Rc::ptr_eq(rc_a, rc_b),

            #[allow(unreachable_patterns)]
            (Nil, _)
            | (_, Nil)
            | (Boolean(_), _)
            | (_, Boolean(_))
            | (Number(_), _)
            | (_, Number(_))
            | (String(_), _)
            | (_, String(_))
            | (NativeFunc(_), _)
            | (_, NativeFunc(_))
            | (LoxFunc(_), _)
            | (_, LoxFunc(_))
            | (Class(_), _)
            | (_, Class(_))
            | (Instance(_), _)
            | (_, Instance(_))
            | (Array(_), _)
            | (_, Array(_)) => false,
        }
    }
}

impl fmt::Display for StringKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StringKind::*;
        write!(
            f,
            "{}",
            match self {
                Static(sym) => {
                    let interner = INTERNER.read().unwrap();
                    interner.resolve(*sym).unwrap().into()
                }
                Dynamic(s) => s.to_string(),
                Cat(a, b) => format!("{}{}", a, b),
            }
        )
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Object::*;
        match self {
            Nil => write!(f, "nil"),
            Boolean(b) => write!(f, "{}", b),
            Number(n) => write!(f, "{}", n),
            String(kind) => write!(f, "{}", kind),
            NativeFunc(func) => write!(f, "[built-in fun {}]", func.name),
            LoxFunc(_func) => write!(f, "[fun]"),
            Class(c) => write!(f, "[class {}]", c),
            Instance(inst) => write!(f, "[instance of {}]", inst.borrow().class),
            Array(vec) => {
                write!(f, "[array")?;
                for item in vec.borrow().iter() {
                    write!(f, " {}", item)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", sym_to_str(&self.name))
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction#{}", self.name)
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Object::*;
        match self {
            Nil => write!(f, "nil"),
            Boolean(b) => write!(f, "{:?}", b),
            Number(_n) => write!(f, "{}", self), // use the standard Display to print integers without the .0
            String(kind) => write!(f, "\"{}\"", kind),
            NativeFunc(func) => write!(f, "[built-in fun {}]", func.name),
            LoxFunc(_func) => write!(f, "[fun]"),
            Class(c) => write!(f, "[class {}]", c),
            Instance(inst) => write!(f, "[instance of {}]", inst.borrow().class),
            Array(vec) => {
                write!(f, "[array")?;
                for item in vec.borrow().iter() {
                    write!(f, " {}", item)?;
                }
                write!(f, "]")
            }
        }
    }
}
