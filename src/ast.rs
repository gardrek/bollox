use crate::scanner::Operator;
use crate::scanner::{ReservedWord, Token, TokenKind};
use crate::store::StoreId;

#[derive(Debug)]
pub enum Literal {
    Nil,
    False,
    True,
    Number(f64),
    StaticString(StoreId),
}

impl Literal {
    pub fn from_token(token: &Token) -> Option<Self> {
        use Literal::*;
        Some(match token.kind() {
            TokenKind::Number(n) => Number(*n),
            TokenKind::StaticString(op) => match op {
                Some(id) => StaticString(*id),
                None => return None,
            },
            TokenKind::Reserved(word) => match word {
                ReservedWord::Nil => Nil,
                ReservedWord::False => False,
                ReservedWord::True => True,
                //ReservedWord::This,
                _ => return None,
            },
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
}
