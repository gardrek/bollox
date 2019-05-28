use crate::scanner::{ReservedWord, Token, TokenKind};
use crate::store::StoreId;

#[derive(Debug)]
pub enum Object {
    Nil,
    Boolean(bool),
    Number(f64),
    DynamicString(String),
    StaticString(StoreId),
}

impl From<Token> for Option<Object> {
    fn from(token: Token) -> Self {
        use Object::*;
        use TokenKind::*;
        Some(match token.kind() {
            TokenKind::Number(value) => Object::Number(*value),
            TokenKind::StaticString(Some(store_id)) => Object::StaticString(*store_id),
            Reserved(word) => match word {
                ReservedWord::True => Boolean(true),
                ReservedWord::False => Boolean(false),
                ReservedWord::Nil => Object::Nil,
                _ => return None,
            },
            _ => return None,
        })
    }
}
