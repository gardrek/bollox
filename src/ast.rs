use crate::object::LoxFunction;
use crate::object::Object;
use crate::source::SourceLocation;
use crate::token::Operator;
use std::fmt;
use string_interner::Sym;

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Stmt {
        Stmt { kind }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Block(Vec<Stmt>),
    Break(Option<Expr>),
    //~ ClassDeclaration(Sym, Option<Sym>, Vec<Stmt>, Vec<Stmt>),
    ClassDeclaration {
        global: bool,
        name: Sym,
        superclass: Option<Sym>,
        methods: Vec<Stmt>,
        associated_funcs: Vec<Stmt>,
    },
    Expr(Expr),
    FunctionDeclaration(bool, Sym, LoxFunction),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Return(Expr),
    VariableDeclaration(bool, Sym, Option<Expr>),
    While(Expr, Box<Stmt>),
}

#[derive(Clone)]
pub struct Expr {
    pub location: SourceLocation,
    pub kind: ExprKind,
}

#[derive(Clone)]
pub enum ExprKind {
    Literal(Object),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
    VariableAccess(Sym),
    Assign(Sym, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),
    LogicalAnd(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    PropertyAccess(Box<Expr>, Sym),
    PropertyAssign(Box<Expr>, Sym, Box<Expr>),
    This,
    Super(Sym),
    ArrayConstructor(Vec<Expr>),
    ArrayConstructorMulti(Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    IndexAssign(Box<Expr>, Box<Expr>, Box<Expr>),
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
            StmtKind::Block(stmts) => {
                write!(f, "(block-stmt")?;
                for st in stmts {
                    write!(f, " {}", st)?;
                }
                write!(f, ")")
            }
            StmtKind::Break(expr) => match expr {
                Some(e) => write!(f, "(break-stmt {})", e),
                None => write!(f, "(break-stmt)"),
            },
            StmtKind::ClassDeclaration { global, name, .. } => {
                let l = if *global { "global" } else { "local" };
                write!(f, "(class-stmt {l} {})", crate::object::sym_to_str(name))
            }
            StmtKind::Expr(expr) => write!(f, "(expr-stmt {})", expr),
            StmtKind::FunctionDeclaration(
                global,
                name,
                LoxFunction {
                    parameters,
                    body,
                    closure: _,
                },
            ) => {
                let l = if *global { "global" } else { "local" };
                write!(f, "(fun-stmt {l} {} (", crate::object::sym_to_str(name))?;
                for p in parameters {
                    write!(f, " {}", crate::object::sym_to_str(p))?;
                }
                write!(f, " ) ")?;
                for st in body {
                    write!(f, " {}", st)?;
                }
                write!(f, ")")
            }
            StmtKind::If(cond, then_block, else_block) => match else_block {
                Some(e) => write!(f, "(if-stmt {} {} {})", cond, then_block, e),
                None => write!(f, "(if-stmt {} {})", cond, then_block),
            },
            StmtKind::Print(expr) => write!(f, "(print-stmt {})", expr),
            StmtKind::Return(expr) => write!(f, "(return-stmt {})", expr),
            StmtKind::VariableDeclaration(global, sym, expr) => {
                let l = if *global { "global" } else { "local" };
                match expr {
                    Some(e) => write!(
                        f,
                        "(var-stmt {} {} {})",
                        l,
                        crate::object::sym_to_str(sym),
                        e
                    ),
                    None => write!(f, "(var-stmt {} {})", l, crate::object::sym_to_str(sym)),
                }
            }
            StmtKind::While(cond, body) => write!(f, "(while-stmt {} {})", cond, body),
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
            LogicalOr(a, b) => write!(f, "(or {} {})", a, b),
            LogicalAnd(a, b) => write!(f, "(and {} {})", a, b),
            Call(callee, args) => {
                write!(f, "(call {}", callee)?;
                for e in args {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")
            }
            PropertyAccess(obj, name) => write!(f, "(property-access {} {:?})", obj, name),
            PropertyAssign(obj, name, value) => {
                write!(f, "(property-assign {} {:?} {})", obj, name, value)
            }
            This => write!(f, "(this)"),
            Super(expr) => write!(f, "(super {:?})", expr),
            ArrayConstructor(exprs) => {
                write!(f, "(array")?;
                for e in exprs {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")
            }
            ArrayConstructorMulti(obj, multi) => write!(f, "(array-multi {} {})", obj, multi),
            Index(obj, index) => write!(f, "(index {} {})", obj, index),
            IndexAssign(obj, index, val) => write!(f, "(index {} {} {})", obj, index, val),
        }
    }
}
