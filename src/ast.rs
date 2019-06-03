use crate::object::Object;
use crate::scanner::Operator;
use crate::scanner::SourceLocation;
use std::fmt;

#[derive(Debug)]
pub struct Stmt {
    location: Option<SourceLocation>,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Self {
            kind,
            location: None,
        }
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
    Print(Expr),
    //VarDeclaration(String),
}

#[derive(Debug)]
pub struct Expr {
    pub location: SourceLocation,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Object),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.kind,
        )
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Expr {} at {}",
            self.kind,
            self.location,
        )
    }
}

impl fmt::Display for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StmtKind::Expr(expr) => write!(f, "Expression statement ({})", expr),
            StmtKind::Print(expr) => write!(f, "Print statement ({})", expr),
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExprKind::*;
        match self {
            Literal(obj) => write!(f, "Literal Expression ({:?})", obj),
            Unary(op, expr) => write!(f, "({:?} {})", op, expr),
            Binary(expr_a, op, expr_b) => write!(f, "({} {:?} {})", expr_a, op, expr_b),
            Grouping(expr) => write!(f, "Grouping Expression ({})", expr),
        }
    }
}
