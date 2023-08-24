use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::object::sym_to_str;
use crate::object::LoxFunction;
use crate::object::Object;

use crate::INTERNER;
use string_interner::Sym;

#[derive(Default)]
pub struct Resolver {
    scopes: Vec<HashMap<Sym, VariableState>>,
    global_scope: HashMap<Sym, VariableState>,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
    function_kind: Option<FunctionKind>,
    class_kind: Option<ClassKind>,
}

#[derive(Clone)]
enum FunctionKind {
    Function,
    Method,
}

#[derive(Clone)]
enum ClassKind {
    Class,
    SubClass,
}

#[derive(Clone)]
enum VariableState {
    Declared,
    Initialized,
    AssignedBeforeUse,
    Used,
    DeclaredConstant,
    InitializedConstant,
    ReassignedConstant,
    UsedConstant,
}

impl Resolver {
    pub fn resolve_all(&mut self, list: &mut [Stmt]) {
        self.begin_scope();
        for s in list {
            self.resolve_statement(s);
        }
        self.end_scope();
        let scope = &std::mem::take(&mut self.global_scope);
        self.evaluate_scope(scope);
    }

    fn resolve_statement(&mut self, stmt: &mut Stmt) {
        use StmtKind::*;
        match &mut stmt.kind {
            Block(block) => {
                self.begin_scope();
                for s in block {
                    self.resolve_statement(s);
                }
                self.end_scope();
            }
            Break(expr) => {
                if let Some(e) = expr {
                    self.resolve_expression(e);
                }
            }
            ClassDeclaration {
                global,
                constant,
                name,
                superclass,
                methods,
                associated_funcs,
            } => {
                let prev_kind = self.class_kind.take();
                self.class_kind = Some(ClassKind::Class);

                if *global {
                    self.declare_global(name, *constant);
                    self.init_variable(name, None);
                } else {
                    self.declare(name, *constant);
                    self.init_variable(name, Some(0));
                }

                if let Some((name, depth)) = superclass {
                    self.class_kind = Some(ClassKind::SubClass);
                    *depth = self.resolve(name);
                    self.use_variable(name, *depth);
                }

                self.begin_scope();

                if superclass.is_some() {
                    let super_sym = {
                        let mut interner = INTERNER.write().unwrap();

                        interner.get_or_intern("super")
                    };

                    self.declare(&super_sym, *constant);
                    self.use_variable(&super_sym, Some(0));
                }

                for method in methods.iter_mut() {
                    match &mut method.kind {
                        StmtKind::FunctionDeclaration { func, .. } => {
                            let declaration = FunctionKind::Method;
                            self.resolve_function(func, declaration);
                        }
                        _ => unreachable!(),
                    }
                }

                self.end_scope();

                for assoc_func in associated_funcs.iter_mut() {
                    match &mut assoc_func.kind {
                        StmtKind::FunctionDeclaration { func, .. } => {
                            let declaration = FunctionKind::Function;
                            self.resolve_function(func, declaration);
                        }
                        _ => unreachable!(),
                    }
                }

                self.class_kind = prev_kind;
            }
            Expr(expr) => self.resolve_expression(expr),
            FunctionDeclaration {
                global,
                constant,
                name,
                func,
            } => {
                if *global {
                    self.declare_global(name, *constant);
                    self.init_variable(name, None);
                } else {
                    self.declare(name, *constant);
                    self.init_variable(name, Some(0));
                }
                self.resolve_function(func, FunctionKind::Function);
            }
            If(cond, body, else_body) => {
                self.resolve_expression(cond);
                self.resolve_statement(body);
                if let Some(s) = else_body {
                    self.resolve_statement(s);
                }
            }
            Print(expr) => self.resolve_expression(expr),
            Return(expr) => self.resolve_expression(expr),
            VariableDeclaration {
                global,
                constant,
                name,
                initializer,
            } => {
                if let Some(e) = initializer {
                    self.resolve_expression(e);
                }
                if *global {
                    self.declare_global(name, *constant);
                    if initializer.is_some() {
                        self.init_variable(name, None);
                    }
                } else {
                    self.declare(name, *constant);
                    if initializer.is_some() {
                        self.init_variable(name, Some(0));
                    }
                }
            }
            While(cond, body) => {
                self.resolve_expression(cond);
                self.resolve_statement(body);
            }
        }
    }

    fn resolve_expression(&mut self, expr: &mut Expr) {
        use ExprKind::*;
        match &mut expr.kind {
            Literal(obj) => {
                if let Object::LoxFunc(func) = obj {
                    self.resolve_function(func, FunctionKind::Function);
                }
            }
            Unary(_op, a) => self.resolve_expression(a),
            Binary(a, _op, b) => {
                self.resolve_expression(a);
                self.resolve_expression(b);
            }
            Grouping(expr) => self.resolve_expression(expr),
            VariableAccess(name, depth) => {
                *depth = self.resolve(name);
                self.use_variable(name, *depth);
            }
            Assign(name, expr, depth) => {
                self.resolve_expression(expr);
                *depth = self.resolve(name);
                self.assign_variable(name, *depth);
            }
            LogicalOr(a, b) => {
                self.resolve_expression(a);
                self.resolve_expression(b);
            }
            LogicalAnd(a, b) => {
                self.resolve_expression(a);
                self.resolve_expression(b);
            }
            Call(callee, args) => {
                self.resolve_expression(callee);
                for e in args {
                    self.resolve_expression(e);
                }
            }
            PropertyAccess(obj, _sym) => self.resolve_expression(obj),
            PropertyAssign(obj, _sym, val) => {
                self.resolve_expression(val);
                self.resolve_expression(obj);
            }
            This => {
                if self.class_kind.is_none() {
                    self.errors
                        .push("Can't use `this` outside a class.".to_string());
                }

                let name = {
                    let mut interner = INTERNER.write().unwrap();
                    interner.get_or_intern("this")
                };

                let depth = self.resolve(&name);
                self.use_variable(&name, depth);
            }
            Super(_sym) => {
                match self.class_kind {
                    None => self
                        .errors
                        .push("Can't use `super` outside a class.".to_string()),
                    Some(ClassKind::Class) => self
                        .errors
                        .push("Can't use `super` in a class with no superclass.".to_string()),
                    _ => (),
                }

                let name = {
                    let mut interner = INTERNER.write().unwrap();
                    interner.get_or_intern("super")
                };

                let depth = self.resolve(&name);
                self.use_variable(&name, depth);
            }
            ArrayConstructor(list) => {
                for e in list {
                    self.resolve_expression(e);
                }
            }
            ArrayConstructorMulti(val, _repeat) => self.resolve_expression(val),
            Index(obj, index) => {
                self.resolve_expression(index);
                self.resolve_expression(obj);
            }
            IndexAssign { obj, index, val } => {
                self.resolve_expression(val);
                self.resolve_expression(index);
                self.resolve_expression(obj);
            }
        }
    }

    fn resolve_function(&mut self, func: &mut LoxFunction, kind: FunctionKind) {
        let enclosing_kind = self.function_kind.take();

        self.function_kind = Some(kind.clone());

        self.begin_scope(); // closure created at function declaration

        if let FunctionKind::Method = kind {
            let this = {
                let mut interner = INTERNER.write().unwrap();

                interner.get_or_intern("this")
            };

            self.declare(&this, true);
            self.use_variable(&this, Some(0)); // supress "unused variable this" everywhere
        }

        self.begin_scope(); // environment created at function call

        for name in func.parameters.iter() {
            self.declare(name, true);
        }

        for s in func.body.iter_mut() {
            self.resolve_statement(s);
        }

        self.end_scope();

        self.end_scope();

        self.function_kind = enclosing_kind;
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            self.evaluate_scope(&scope);
        }
    }

    fn evaluate_scope(&mut self, scope: &HashMap<Sym, VariableState>) {
        for (name, usage) in scope.iter() {
            use VariableState::*;
            match usage {
                Declared | Initialized | DeclaredConstant | InitializedConstant => {
                    let name = sym_to_str(name);
                    if let Some(b'_') = name.as_bytes().first() {
                        // do nothing
                    } else {
                        self.warnings.push(format!("Unused variable `{}`.", name))
                    }
                }
                AssignedBeforeUse => self.warnings.push(format!(
                    "Variable `{}` re-assigned before use.",
                    sym_to_str(name)
                )),
                ReassignedConstant => self
                    .errors
                    .push(format!("Constant `{}` re-assigned.", sym_to_str(name))),
                Used | UsedConstant => (),
            }
        }
    }

    fn declare(&mut self, name: &Sym, constant: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                self.errors
                    .push("Already a variable with this name in this scope.".to_string());
            }
            let declared = if constant {
                VariableState::DeclaredConstant
            } else {
                VariableState::Declared
            };
            scope.insert(*name, declared);
        }
    }

    fn declare_global(&mut self, name: &Sym, constant: bool) {
        let declared = if constant {
            VariableState::DeclaredConstant
        } else {
            VariableState::Declared
        };
        self.global_scope.insert(*name, declared);
    }

    fn init_in_scope(scope: &mut HashMap<Sym, VariableState>, name: &Sym) -> Option<()> {
        if let Some(state) = scope.get_mut(name) {
            use VariableState::*;
            match state {
                Declared => *state = Initialized,
                Initialized => *state = AssignedBeforeUse,
                Used | AssignedBeforeUse => (),
                DeclaredConstant => *state = InitializedConstant,
                InitializedConstant | UsedConstant => *state = ReassignedConstant,
                ReassignedConstant => (),
            }
            Some(())
        } else {
            None
        }
    }

    fn assign_in_scope(scope: &mut HashMap<Sym, VariableState>, name: &Sym) -> Option<()> {
        if let Some(state) = scope.get_mut(name) {
            use VariableState::*;
            match state {
                Declared => *state = Used, // ??
                Initialized => *state = AssignedBeforeUse,
                Used | AssignedBeforeUse => (),
                DeclaredConstant => *state = InitializedConstant,
                InitializedConstant | UsedConstant => *state = ReassignedConstant,
                ReassignedConstant => (),
            }

            Some(())
        } else {
            None
        }
    }

    fn use_in_scope(scope: &mut HashMap<Sym, VariableState>, name: &Sym) -> Option<()> {
        if let Some(state) = scope.get_mut(name) {
            use VariableState::*;
            match state {
                DeclaredConstant | Declared | Initialized | Used => *state = Used,
                InitializedConstant | UsedConstant => *state = UsedConstant,
                ReassignedConstant | AssignedBeforeUse => (),
            }

            Some(())
        } else {
            None
        }
    }

    fn init_variable(&mut self, name: &Sym, depth: Option<usize>) {
        if let Some(mut depth) = depth {
            let scope = 'scope: {
                for scope in self.scopes.iter_mut().rev() {
                    if depth == 0 {
                        break 'scope Some(scope);
                    }
                    depth -= 1;
                }

                None
            };

            if let Some(scope) = scope {
                if Self::init_in_scope(scope, name).is_none() {
                    self.errors.push(format!(
                        "ICE No variable named {} in this scope.",
                        sym_to_str(name)
                    ));
                }
            }
        } else if Self::init_in_scope(&mut self.global_scope, name).is_none() {
            self.errors.push(format!(
                "ICE No variable named {} in global scope.",
                sym_to_str(name)
            ));
        }
    }

    fn assign_variable(&mut self, name: &Sym, depth: Option<usize>) {
        if let Some(mut depth) = depth {
            let scope = 'scope: {
                for scope in self.scopes.iter_mut().rev() {
                    if depth == 0 {
                        break 'scope Some(scope);
                    }
                    depth -= 1;
                }

                None
            };

            if let Some(scope) = scope {
                if Self::assign_in_scope(scope, name).is_none() {
                    self.errors.push(format!(
                        "ICE No variable named {} in this scope.",
                        sym_to_str(name)
                    ));
                }
            }
        } else {
            let _ = Self::assign_in_scope(&mut self.global_scope, name);
        }
    }

    fn use_variable(&mut self, name: &Sym, depth: Option<usize>) {
        if let Some(mut depth) = depth {
            let scope = 'scope: {
                for scope in self.scopes.iter_mut().rev() {
                    if depth == 0 {
                        break 'scope Some(scope);
                    }
                    depth -= 1;
                }

                None
            };

            if let Some(scope) = scope {
                if Self::use_in_scope(scope, name).is_none() {
                    self.errors.push(format!(
                        "ICE No variable named {} in this scope.",
                        sym_to_str(name)
                    ));
                }
            }
        } else {
            let _ = Self::use_in_scope(&mut self.global_scope, name);
        }
    }

    fn resolve(&self, name: &Sym) -> Option<usize> {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                return Some(depth);
            }
        }

        None
    }
}
