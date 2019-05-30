use crate::scanner::{ReservedWord, Token, TokenKind};
use crate::store::{StoreId, Store};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Nil,
    Boolean(bool),
    Number(f64),
    //DynamicString(String),
    StaticString(StoreId),
}

impl Object {
    pub fn new_from_token(token: &Token) -> Option<Object> {
        use Object::*;
        use TokenKind::*;
        Some(match token.kind() {
            TokenKind::Number(value) => Object::Number(*value),
            TokenKind::StaticString(store_id) => Object::StaticString(*store_id),
            Reserved(word) => match word {
                ReservedWord::True => Boolean(true),
                ReservedWord::False => Boolean(false),
                ReservedWord::Nil => Object::Nil,
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

        /* Alternative truthiness rules
            Number(n) => n != 0.0,
            DynamicString(ref s) => !s.is_empty(),
            // NOTE: this relies on the behavior that the empty string is assigned to store id 0
            StaticString(id) => !id.is_zero(),
        */
    }

    /*
    pub fn as_number(&self) -> Option<f64> {
        use Object::*;
        Some(match self {
            Number(n) => *n,
            _ => return None,
        })
    }
    */

    pub fn as_string<'a>(&self, static_store: &'a Store<String>) -> Option<&'a String> {
        use Object::*;
        match self {
            // NOTE: returns None if id is out of range. maybe should change?
            StaticString(id) => static_store.get(*id),
            //DynamicString(&s) => s,
            _ => return None,
        }
    }
}

use std::fmt;

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Object::*;
        match self {
            Nil => write!(f, "nil"),
            Boolean(b) => write!(f, "{}", b),
            Number(n) => write!(f, "{}", n),
            StaticString(id) => write!(f, "[StaticString:{:?}]", id),
        }
    }
}
