use std::fmt;

use crate::token::{ReservedWord, Token, TokenKind};
use crate::INTERNER;
use crate::interpreter::Interpreter;
use string_interner::Sym;

#[derive(Clone)]
pub enum Object {
    Nil,
    Boolean(bool),
    Number(f64),
    String(StringKind),
    Callable(Callable),
}

#[derive(Debug, Clone)]
pub enum StringKind {
    //~ Dynamic(String),
    Static(Sym),
    Cat(Box<StringKind>, Box<StringKind>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    //~ Native(Box<Fn(&mut Interpreter, Vec<Object>) -> Object>)
}

impl Callable {
    pub fn arity(&self) -> usize {
        match self {
            _ => todo!(),
        }
    }

    pub fn call(&mut self, _interpreter: &mut Interpreter, _arguments: Vec<Object>) -> Object {
        match self {
            _ => todo!(),
        }
    }
}

impl StringKind {
    pub fn concat(self, other: Self) -> Self {
        use StringKind::*;
        match (&self, &other) {
            //~ (Dynamic(_), _) | (_, Dynamic(_)) => unimplemented!(),
            (_, _) => Cat(Box::new(self), Box::new(other)),
        }
    }
}

impl Object {
    pub fn from_token(token: &Token) -> Option<Object> {
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

    pub fn is_callable(&self) -> bool {
        use Object::*;
        match self {
            Nil | Boolean(_) | Number(_) | String(_) => false,
            Callable(_) => true,
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
                //~ (Dynamic(a), Dynamic(b)) => a == b,
                //~ (Dynamic(a), _) => a == &b_kind.to_string(),
                //~ (_, Dynamic(b)) => &a_kind.to_string() == b,
                (Cat(_, _), Cat(_, _)) | (Cat(_, _), _) | (_, Cat(_, _)) => {
                    a_kind.to_string() == b_kind.to_string()
                }
            },
            (Callable(a), Callable(b)) => a == b,
            (Nil, _) => false,
            (_, Nil) => false,
            (Boolean(_), _) => false,
            (_, Boolean(_)) => false,
            (Number(_), _) => false,
            (_, Number(_)) => false,
            (Callable(_), _) => false,
            (_, Callable(_)) => false,
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
                //~ Dynamic(s) => s.clone(),
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
            Callable(c) => write!(f, "{:?}", c),
        }
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
            Callable(c) => write!(f, "{:?}", c),
        }
    }
}
