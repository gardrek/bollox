use crate::object::Object;
use crate::scanner::Operator;

#[derive(Debug)]
pub enum Expr {
    Literal(Object),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
}

/* TODO: implement somethign like this in order to point to the location of errors.
use crate::scanner::SourceLocation;
#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    location: SourceLocation,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Object),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
}
*/
