use crate::object::Object;
use crate::source::SourceLocation;
use crate::token::Operator;
use std::fmt;
use string_interner::Sym;

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Stmt {
        Stmt { kind }
    }
}

pub enum StmtKind {
    Expr(Expr),
    Print(Expr),

    // the Sym is the variable name, the Expr is the initializer
    // TODO: Maybe split this into its own enum when there's more declarations?
    VariableDeclaration(Sym, Option<Expr>),
}

pub struct Expr {
    pub location: SourceLocation,
    pub kind: ExprKind,
}

pub enum ExprKind {
    Literal(Object),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
    VariableAccess(Sym),
    Assign(Sym, Box<Expr>),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind,)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Expr {} at {}", self.kind, self.location,)
    }
}

impl fmt::Display for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StmtKind::Expr(expr) => write!(f, "(expr-stmt {})", expr),
            StmtKind::Print(expr) => write!(f, "(print-stmt {})", expr),
            StmtKind::VariableDeclaration(sym, expr) => match expr {
                Some(e) => write!(f, "(Variable-Declaration-statement {:?} = {})", sym, e),
                None => write!(f, "(Variable-Declaration-statement {:?})", sym),
            },
        }
    }
}

impl fmt::Debug for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StmtKind::Expr(expr) => write!(f, "Expression statement ({})", expr),
            StmtKind::Print(expr) => write!(f, "Print statement ({})", expr),
            StmtKind::VariableDeclaration(sym, expr) => match expr {
                Some(e) => write!(f, "Variable Declaration statement ({:?} = {})", sym, e),
                None => write!(f, "Variable Declaration statement ({:?})", sym),
            },
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExprKind::*;
        match self {
            Literal(obj) => write!(f, "{:?}", obj),
            Unary(op, expr) => write!(f, "({} {})", op, expr),
            Binary(expr_a, op, expr_b) => write!(f, "({} {} {})", op, expr_a, expr_b),
            Grouping(expr) => write!(f, "(group-expr {})", expr),
            VariableAccess(sym) => write!(f, "(var-access {:?})", sym),
            Assign(sym, expr) => write!(f, "(assign {:?} {})", sym, expr),
        }
    }
}

impl fmt::Debug for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExprKind::*;
        match self {
            Literal(obj) => write!(f, "Literal Expression ({:?})", obj),
            Unary(op, expr) => write!(f, "({:?} {})", op, expr),
            Binary(expr_a, op, expr_b) => write!(f, "({} {:?} {})", expr_a, op, expr_b),
            Grouping(expr) => write!(f, "Grouping Expression ({})", expr),
            VariableAccess(sym) => write!(f, "(var-access {:?})", sym),
            Assign(sym, expr) => write!(f, "(assign {:?} {})", sym, expr),
        }
    }
}
