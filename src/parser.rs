use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::object::Object;
use crate::scanner::Scanner;
use crate::source::SourceLocation;
use crate::token::{Operator, ReservedWord, Token, TokenKind};

const MAX_ARGUMENTS: usize = 255;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    ExpectedIdentifier,
    ExpectedLeftBrace,
    ExpectedToken(Vec<TokenKind>, Option<Token>),
    Ice(&'static str),
    MaxArgumentsExceeded,
    UnexpectedToken(Token),
    InvalidAssignmentTarget,
    ScannerError(Box<crate::result::Error>),
    InvalidConstructor,
}

impl From<ParseError> for crate::result::Error {
    fn from(other: ParseError) -> crate::result::Error {
        crate::result::Error::Parser(other)
    }
}

impl core::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        use ParseErrorKind::*;

        match self {
            ExpectedIdentifier => write!(f, "Expected Identifier"),
            ExpectedLeftBrace => write!(f, "Expected Starting Brace"),
            ExpectedToken(expected, found) => {
                if expected.len() == 1 {
                    write!(f, "Expected `{}`, ", expected[0])?;
                } else {
                    write!(f, "Expected one of ")?;
                    for e in expected {
                        write!(f, "`{}`, ", e)?;
                    }
                }

                match found {
                    None => write!(f, "found end of stream"),
                    Some(token) => write!(f, "found `{}`", token),
                }
            }
            Ice(s) => write!(f, "Internal Compiler Error: {}", s),
            MaxArgumentsExceeded => write!(
                f,
                "Max of {} function call arguments exceeded",
                MAX_ARGUMENTS
            ),
            UnexpectedToken(t) => write!(f, "Unexpected Token `{}`", t),
            InvalidAssignmentTarget => write!(f, "Invalid Assignment Target"),
            ScannerError(e) => write!(f, "Scanner Error {}", e),
            InvalidConstructor => write!(f, "Invalid Constructor"),
        }
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    pub location: SourceLocation,
}

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{} at {}", self.kind, self.location)
    }
}

pub struct Parser {
    scanner: Scanner,
    previous: Option<Token>,
    current: Option<Token>,
    pub errors: Vec<ParseError>,
    at_end: bool,
    compatibility: bool,
    scope_depth: usize,
}

impl Parser {
    pub fn new(scanner: Scanner, compatibility: bool) -> Self {
        let mut p = Self {
            scanner,
            previous: None,
            current: None,
            errors: vec![],
            at_end: false,
            compatibility,
            scope_depth: 0,
        };

        p.init();

        p
    }

    pub fn push_source_string(&mut self, s: &str) {
        self.scanner.push_source_string(s);
        self.init();
    }

    pub fn parse_all(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while !self.is_at_end() {
            if let Some(stmt) = self.parse_next() {
                statements.push(stmt?);
            }
        }
        Ok(statements)
    }

    fn init(&mut self) {
        self.current = self.next_token();
        self.at_end = self.current.is_none();
    }

    fn is_at_end(&self) -> bool {
        self.at_end
    }

    // equivalent of jlox's parse()
    fn parse_next(&mut self) -> Option<Result<Stmt, ParseError>> {
        if self.is_at_end() {
            return None;
        }

        match self.decorated_declaration() {
            Ok(stmt) => Some(Ok(stmt)),
            Err(err) => {
                self.report(err);
                self.synchronize();
                None
            }
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.scanner.next() {
            Some(r) => match r {
                Ok(t) => Some(t),
                Err(e) => {
                    let location = match &self.current {
                        Some(t) => t.location.clone(),
                        None => SourceLocation::bullshit(),
                    };
                    self.report(ParseError {
                        location,
                        kind: ParseErrorKind::ScannerError(Box::new(e)),
                    });
                    None
                }
            },
            None => None,
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        self.previous = self.current.clone();
        self.current = self.next_token();
        if self.current.is_none() {
            self.at_end = true;
        }
        self.previous.as_ref()
    }

    fn check(&self, kinds: &[TokenKind]) -> bool {
        match self.peek() {
            Some(t) => t.in_kinds(kinds),
            None => false,
        }
    }

    fn check_advance(&mut self, kinds: &[TokenKind]) -> Option<&Token> {
        if self.check(kinds) {
            self.advance()
        } else {
            None
        }
    }

    fn advance_if_match(&mut self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(t) => {
                if t.kind.same_kind(&kind) {
                    self.advance();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn peek_previous(&self) -> Option<&Token> {
        self.previous.as_ref()
    }

    fn consume(
        &mut self,
        kinds: &[TokenKind],
        err: ParseErrorKind,
    ) -> Result<Option<&Token>, ParseError> {
        if self.check(kinds) {
            Ok(self.advance())
        } else {
            Err(self.error(err))
        }
    }

    fn consume_expected(&mut self, kinds: &[TokenKind]) -> Result<Option<&Token>, ParseError> {
        self.consume(
            kinds,
            ParseErrorKind::ExpectedToken(kinds.to_owned(), self.peek().cloned()),
        )
    }

    fn consume_identifier(&mut self) -> Result<string_interner::Sym, ParseError> {
        if let Some(t) = self.peek() {
            if let TokenKind::Identifier(sym) = &t.kind {
                let sym = *sym;
                self.advance();
                return Ok(sym);
            }
        }

        Err(self.error(ParseErrorKind::ExpectedIdentifier))
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        let location = match self.peek() {
            Some(t) => t.location.clone(),
            None => match self.peek_previous() {
                Some(t) => t.location.clone(),
                None => SourceLocation::bullshit(),
            },
        };

        ParseError { kind, location }
    }

    fn report(&mut self, error: ParseError) {
        self.errors.push(error)
    }

    fn synchronize(&mut self) {
        self.advance();

        while let Some(token) = self.peek() {
            if let Some(t) = self.peek_previous() {
                if let TokenKind::Op(Operator::Semicolon) = t.kind {
                    break;
                }
            }

            use ReservedWord::*;
            if let TokenKind::Reserved(keyword) = &token.kind {
                match keyword {
                    Class | Fun | Var | For | If | While | Print | Return | Break | Switch => break,
                    _ => (),
                }
            }

            self.advance();
        }
    }

    /// This is a helper function used by the separate rules
    /// for each binary operator function.
    fn binary_op(
        &mut self,
        get_argument: fn(&mut Parser) -> Result<Expr, ParseError>,
        kinds: &[TokenKind],
    ) -> Result<Expr, ParseError> {
        let mut expr = get_argument(self)?;
        let mut location = expr.location.clone();

        while let Some(op_token) = self.check_advance(kinds) {
            //~ let op_token = self.peek_previous().unwrap();
            location = location.combine(&op_token.location);
            let op = op_token.to_operator();
            let right = get_argument(self)?;
            location = location.combine(&right.location);
            expr = Expr {
                location: location.clone(),
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
            };
        }

        Ok(expr)
    }
}

/// ### Recursive Descent
/// Most of the following functions each represent one rule of the language's grammar
impl Parser {
    fn decorated_declaration(&mut self) -> Result<Stmt, ParseError> {
        if let Some(token) = self.check_advance(&[
            TokenKind::Reserved(ReservedWord::Global),
            TokenKind::Reserved(ReservedWord::Local),
        ]) {
            match &token.kind {
                TokenKind::Reserved(word) => {
                    use ReservedWord::*;
                    match word {
                        Global => self.declaration(true),
                        Local => self.declaration(false),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }
        } else if self.check(&[
            TokenKind::Reserved(ReservedWord::Class),
            TokenKind::Reserved(ReservedWord::Fun),
            TokenKind::Reserved(ReservedWord::Var),
        ]) {
            // here is where we implement backwards compatibility with Lox
            // in compatibility mode, it should declare a global when in the "global" scope
            self.declaration(self.compatibility && (self.scope_depth == 0))
        } else {
            self.statement()
        }
    }

    fn declaration(&mut self, global: bool) -> Result<Stmt, ParseError> {
        if let Some(token) = self.check_advance(&[
            TokenKind::Reserved(ReservedWord::Class),
            TokenKind::Reserved(ReservedWord::Fun),
            TokenKind::Reserved(ReservedWord::Var),
        ]) {
            match &token.kind {
                TokenKind::Reserved(word) => {
                    use ReservedWord::*;
                    match word {
                        Class => self.class_declaration(global),
                        Fun => self.function_declaration(global),
                        Var => self.variable_declaration(global),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }
        } else {
            return Err(self.error(ParseErrorKind::ExpectedToken(
                vec![
                    TokenKind::Reserved(ReservedWord::Class),
                    TokenKind::Reserved(ReservedWord::Fun),
                    TokenKind::Reserved(ReservedWord::Var),
                ],
                self.peek().cloned(),
            )));
        }
    }

    fn class_declaration(&mut self, global: bool) -> Result<Stmt, ParseError> {
        let name = self.consume_identifier()?;

        let superclass = if self.advance_if_match(TokenKind::Op(Operator::Less)) {
            let name = self.consume_identifier()?;

            Some(name)
        } else {
            None
        };

        self.consume_expected(&[TokenKind::LeftBrace])?;

        let mut methods = vec![];
        let mut associated_funcs = vec![];

        while !self.check(&[TokenKind::RightBrace]) && !self.is_at_end() {
            if self.advance_if_match(TokenKind::Reserved(ReservedWord::Class)) {
                associated_funcs.push(self.function_declaration(false)?);
            } else {
                methods.push(self.function_declaration(false)?);
            }
        }

        self.consume_expected(&[TokenKind::RightBrace])?;

        Ok(Stmt::new(StmtKind::ClassDeclaration {
            global,
            name,
            superclass,
            methods,
            associated_funcs,
        }))
    }

    fn function_declaration(&mut self, global: bool) -> Result<Stmt, ParseError> {
        let name = self.consume_identifier()?;

        Ok(Stmt::new(StmtKind::FunctionDeclaration(
            global,
            name,
            self.function()?,
        )))
    }

    fn function(&mut self) -> Result<crate::object::LoxFunction, ParseError> {
        self.consume_expected(&[TokenKind::LeftParen])?;

        let mut parameters = vec![];

        if !self.check(&[TokenKind::RightParen]) {
            loop {
                parameters.push(self.consume_identifier()?);

                if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                    break;
                }
            }
        }

        if parameters.len() > MAX_ARGUMENTS {
            self.report(self.error(ParseErrorKind::MaxArgumentsExceeded));
        }

        self.consume_expected(&[TokenKind::RightParen])?;

        self.consume_expected(&[TokenKind::LeftBrace])?;

        let body = self.declaration_block()?;

        Ok(crate::object::LoxFunction {
            parameters,
            body,
            closure: Default::default(),
        })
    }

    fn variable_declaration(&mut self, global: bool) -> Result<Stmt, ParseError> {
        let sym = self
            .peek()
            .and_then(|t| match &t.kind {
                TokenKind::Identifier(sym) => Some(*sym),
                _ => None,
            })
            .ok_or(self.error(ParseErrorKind::ExpectedIdentifier))?;

        self.advance();

        let initializer = if self.advance_if_match(TokenKind::Op(Operator::Equal)) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume_expected(&[TokenKind::Op(Operator::Semicolon)])?;

        Ok(Stmt::new(StmtKind::VariableDeclaration(
            global,
            sym,
            initializer,
        )))
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if let Some(token) = self.check_advance(&[
            TokenKind::Reserved(ReservedWord::Break),
            TokenKind::Reserved(ReservedWord::For),
            TokenKind::Reserved(ReservedWord::If),
            TokenKind::Reserved(ReservedWord::Print),
            TokenKind::Reserved(ReservedWord::Return),
            TokenKind::Reserved(ReservedWord::Switch),
            TokenKind::Reserved(ReservedWord::While),
        ]) {
            match &token.kind {
                TokenKind::Reserved(word) => {
                    use ReservedWord::*;
                    match word {
                        Break => self.break_statement(),
                        For => self.for_statement(),
                        If => self.if_statement(),
                        Print => self.print_statement(),
                        Return => self.return_statement(),
                        Switch => self.switch_statement(),
                        While => self.while_statement(),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }
        } else {
            if self.advance_if_match(TokenKind::LeftBrace) {
                return Ok(Stmt::new(StmtKind::Block(self.declaration_block()?)));
            }

            self.expression_statement()
        }
    }

    fn break_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume_expected(&[TokenKind::Op(Operator::Semicolon)])?;

        Ok(Stmt::new(StmtKind::Break(None)))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check(&[TokenKind::LeftParen]) {
            self.c_style_for_statement()
        } else {
            self.iterator_for_statement()
        }
    }

    fn c_style_for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume_expected(&[TokenKind::LeftParen])?;

        let initializer = if self.advance_if_match(TokenKind::Op(Operator::Semicolon)) {
            None
        } else if self.advance_if_match(TokenKind::Reserved(ReservedWord::Var)) {
            Some(self.variable_declaration(false)?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if self.check(&[TokenKind::Op(Operator::Semicolon)]) {
            None
        } else {
            Some(self.expression()?)
        };

        let condition_location = self.peek_previous().unwrap().location.clone();

        self.consume_expected(&[TokenKind::Op(Operator::Semicolon)])?;

        let increment = if self.check(&[TokenKind::RightParen]) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume_expected(&[TokenKind::RightParen])?;

        let body = self.statement()?;

        let mut body = vec![body];

        if let Some(inc) = increment {
            body.push(Stmt::new(StmtKind::Expr(inc)));
        }

        let body = Stmt::new(StmtKind::Block(body));

        let condition = match condition {
            Some(c) => c,
            None => Expr {
                location: condition_location,
                kind: ExprKind::Literal(Object::Boolean(true)),
            },
        };

        let body = Stmt::new(StmtKind::While(condition, Box::new(body)));

        Ok(match initializer {
            Some(init) => Stmt::new(StmtKind::Block(vec![init, body])),
            None => body,
        })
    }

    fn iterator_for_statement(&mut self) -> Result<Stmt, ParseError> {
        let counter_name = self.consume_identifier()?;

        self.consume_expected(&[TokenKind::Reserved(ReservedWord::In)])?;

        let iter_expr = self.expression()?;

        let body = if self.advance_if_match(TokenKind::LeftBrace) {
            Box::new(Stmt::new(StmtKind::Block(self.declaration_block()?)))
        } else {
            return Err(self.error(ParseErrorKind::ExpectedLeftBrace));
        };

        let iter_name = {
            let mut interner = crate::INTERNER.write().unwrap();
            // use a keyword so as not to shadow any existing variable
            // (except other for loops, which should be fine)
            interner.get_or_intern("for")
        };

        let iter_declaration = Stmt::new(StmtKind::VariableDeclaration(
            false,
            iter_name,
            Some(iter_expr),
        ));

        let literal_true = Expr {
            location: SourceLocation::bullshit(),
            kind: ExprKind::Literal(Object::Boolean(true)),
        };

        let counter_declaration = Stmt::new(StmtKind::VariableDeclaration(
            false,
            counter_name,
            Some(literal_true),
        ));

        let counter_function = Expr {
            location: SourceLocation::bullshit(),
            kind: ExprKind::VariableAccess(iter_name),
        };

        let counter_expr = Expr {
            location: SourceLocation::bullshit(),
            kind: ExprKind::Call(Box::new(counter_function), vec![]),
        };

        let counter_next = Stmt::new(StmtKind::Expr(Expr {
            location: SourceLocation::bullshit(),
            kind: ExprKind::Assign(counter_name, Box::new(counter_expr)),
        }));

        let condition = Expr {
            location: SourceLocation::bullshit(),
            kind: ExprKind::VariableAccess(counter_name),
        };

        let if_statement = Stmt::new(StmtKind::If(condition.clone(), body, None));

        let while_body = Box::new(Stmt::new(StmtKind::Block(vec![counter_next, if_statement])));

        let while_loop = Stmt::new(StmtKind::While(condition, while_body));

        Ok(Stmt::new(StmtKind::Block(vec![
            iter_declaration,
            counter_declaration,
            while_loop,
        ])))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        let condition = self.expression()?;

        self.scope_depth += 1;

        let then_block = match condition.kind {
            ExprKind::Grouping(_) => Box::new(self.statement()?),
            _ => {
                if self.advance_if_match(TokenKind::LeftBrace) {
                    Box::new(Stmt::new(StmtKind::Block(self.statement_block()?)))
                } else {
                    return Err(self.error(ParseErrorKind::ExpectedLeftBrace));
                }
            }
        };

        let else_block = if self.advance_if_match(TokenKind::Reserved(ReservedWord::Else)) {
            if self.advance_if_match(TokenKind::LeftBrace) {
                Some(Box::new(Stmt::new(StmtKind::Block(
                    self.statement_block()?,
                ))))
            } else if self.advance_if_match(TokenKind::Reserved(ReservedWord::If)) {
                Some(Box::new(self.if_statement()?))
            } else {
                Some(Box::new(self.statement()?))
                /*
                return Err(self.error(ParseErrorKind::ExpectedToken(
                    vec![TokenKind::LeftBrace, TokenKind::Reserved(ReservedWord::If)],
                    self.peek().cloned(),
                )));
                */
            }
        } else {
            None
        };

        self.scope_depth -= 1;

        Ok(Stmt::new(StmtKind::If(condition, then_block, else_block)))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume_expected(&[TokenKind::Op(Operator::Semicolon)])?;
        Ok(Stmt::new(StmtKind::Print(expr)))
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let location = self.peek().unwrap().location.clone();

        let expr = if self.check(&[TokenKind::Op(Operator::Semicolon)]) {
            Expr {
                location,
                kind: ExprKind::Literal(Object::Nil),
            }
        } else {
            self.expression()?
        };

        self.consume_expected(&[TokenKind::Op(Operator::Semicolon)])?;

        Ok(Stmt::new(StmtKind::Return(expr)))
    }

    fn switch_statement(&mut self) -> Result<Stmt, ParseError> {
        let subject = self.expression()?;

        let subject_name = {
            let mut interner = crate::INTERNER.write().unwrap();
            interner.get_or_intern("switch")
        };

        let subject_access = Expr {
            location: subject.location.clone(),
            kind: ExprKind::VariableAccess(subject_name),
        };

        let subject_declaration = Stmt::new(StmtKind::VariableDeclaration(
            false,
            subject_name,
            Some(subject),
        ));

        if !self.advance_if_match(TokenKind::LeftBrace) {
            return Err(self.error(ParseErrorKind::ExpectedLeftBrace));
        }

        let mut branches = vec![];

        while !self.check(&[
            TokenKind::RightBrace,
            TokenKind::Reserved(ReservedWord::Else),
        ]) && !self.is_at_end()
        {
            let branch = self.switch_arm(&subject_access)?;

            branches.push(branch);
        }

        let else_branch = if self.advance_if_match(TokenKind::Reserved(ReservedWord::Else)) {
            self.consume_expected(&[TokenKind::LeftBrace])?;

            let body = Box::new(Stmt::new(StmtKind::Block(self.statement_block()?)));

            Some(body)
        } else {
            None
        };

        self.consume_expected(&[TokenKind::RightBrace])?;

        let mut body: Option<Stmt> = None;

        while let Some(branch) = branches.pop() {
            let else_branch = match body {
                Some(previous_branch) => Some(Box::new(previous_branch)),
                None => else_branch.clone(),
            };

            match branch.kind {
                StmtKind::If(conditional, then_branch, _) => {
                    body = Some(Stmt::new(StmtKind::If(
                        conditional,
                        then_branch,
                        else_branch,
                    )));
                }
                _ => unreachable!(),
            }
        }

        let body = match body {
            Some(b) => vec![subject_declaration, b],
            None => vec![subject_declaration],
        };

        Ok(Stmt::new(StmtKind::Block(body)))
    }

    fn switch_arm(&mut self, subject_access: &Expr) -> Result<Stmt, ParseError> {
        let mut cases = vec![];
        while !self.check(&[TokenKind::LeftBrace]) && !self.is_at_end() {
            cases.push(self.expression()?);

            if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                break;
            }
        }

        self.consume_expected(&[TokenKind::LeftBrace])?;

        let body = self.statement_block()?;

        let mut conditional: Option<Expr> = None;

        while let Some(case) = cases.pop() {
            let eq_expr = Expr {
                location: case.location.clone(),
                kind: ExprKind::Binary(
                    Box::new(subject_access.clone()),
                    Operator::EqualEqual,
                    Box::new(case.clone()),
                ),
            };

            conditional = match conditional {
                Some(cond) => Some(Expr {
                    location: case.location.clone(),
                    kind: ExprKind::LogicalOr(Box::new(eq_expr), Box::new(cond.clone())),
                }),
                None => Some(eq_expr),
            };
        }

        let conditional = match conditional {
            Some(c) => c,
            None => unreachable!(),
        };

        Ok(Stmt::new(StmtKind::If(
            conditional,
            Box::new(Stmt::new(StmtKind::Block(body))),
            None,
        )))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        let condition = self.expression()?;

        self.scope_depth += 1;

        let body = match condition.kind {
            ExprKind::Grouping(_) => Box::new(self.statement()?),
            _ => {
                if self.advance_if_match(TokenKind::LeftBrace) {
                    Box::new(Stmt::new(StmtKind::Block(self.statement_block()?)))
                } else {
                    return Err(self.error(ParseErrorKind::ExpectedLeftBrace));
                }
            }
        };

        self.scope_depth -= 1;

        Ok(Stmt::new(StmtKind::While(condition, body)))
    }

    fn declaration_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];

        self.scope_depth += 1;

        while !self.check(&[TokenKind::RightBrace]) && !self.is_at_end() {
            stmts.push(self.decorated_declaration()?);
        }

        self.scope_depth -= 1;

        self.consume_expected(&[TokenKind::RightBrace])?;

        Ok(stmts)
    }

    fn statement_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];

        self.scope_depth += 1;

        while !self.check(&[TokenKind::RightBrace]) && !self.is_at_end() {
            // we don't do a separate statement block for `if` etc because it enables some useful idioms
            //~ stmts.push(self.statement()?);
            stmts.push(self.decorated_declaration()?);
        }

        self.scope_depth -= 1;

        self.consume_expected(&[TokenKind::RightBrace])?;

        Ok(stmts)
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume_expected(&[TokenKind::Op(Operator::Semicolon)])?;
        Ok(Stmt::new(StmtKind::Expr(expr)))
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;

        if let Some(op_token) = self.check_advance(&[
            TokenKind::Op(Operator::Equal),
            TokenKind::Op(Operator::MinusEqual),
            TokenKind::Op(Operator::PlusEqual),
            TokenKind::Op(Operator::SlashEqual),
            TokenKind::Op(Operator::StarEqual),
            TokenKind::Op(Operator::PercentEqual),
        ]) {
            let token = op_token.clone();

            let value = Box::new(self.assignment()?);

            let r_expr = match &token.kind {
                TokenKind::Op(op) => match op {
                    Operator::Equal => value,
                    a => Box::new(Expr {
                        location: expr.location.clone(),
                        kind: ExprKind::Binary(
                            Box::new(expr.clone()),
                            a.binary_from_combined(),
                            value,
                        ),
                    }),
                },
                _ => unreachable!(),
            };

            return Ok(match expr.kind {
                ExprKind::VariableAccess(name) => Expr {
                    location: expr.location,
                    kind: ExprKind::Assign(name, r_expr),
                },
                ExprKind::PropertyAccess(obj, name) => Expr {
                    location: expr.location,
                    kind: ExprKind::PropertyAssign(obj, name, r_expr),
                },
                ExprKind::Index(obj, index) => Expr {
                    location: expr.location,
                    kind: ExprKind::IndexAssign(obj, index, r_expr),
                },

                // TODO: Can we report and synchronize here?
                _ => return Err(self.error(ParseErrorKind::InvalidAssignmentTarget)),
            });
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;

        while let Some(operator) = self.check_advance(&[TokenKind::Reserved(ReservedWord::Or)]) {
            let location = operator.location.clone();

            let right = self.logical_and()?;

            expr = Expr {
                location,
                kind: ExprKind::LogicalOr(Box::new(expr), Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(operator) = self.check_advance(&[TokenKind::Reserved(ReservedWord::And)]) {
            let location = operator.location.clone();

            let right = self.equality()?;

            expr = Expr {
                location,
                kind: ExprKind::LogicalAnd(Box::new(expr), Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::comparison,
            &[
                TokenKind::Op(Operator::EqualEqual),
                TokenKind::Op(Operator::BangEqual),
            ],
        )
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::addition,
            &[
                TokenKind::Op(Operator::Greater),
                TokenKind::Op(Operator::GreaterEqual),
                TokenKind::Op(Operator::Less),
                TokenKind::Op(Operator::LessEqual),
            ],
        )
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::multiplication,
            &[
                TokenKind::Op(Operator::Minus),
                TokenKind::Op(Operator::Plus),
            ],
        )
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::unary,
            &[
                TokenKind::Op(Operator::Slash),
                TokenKind::Op(Operator::Star),
                TokenKind::Op(Operator::Percent),
            ],
        )
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op_token) = self.check_advance(&[
            TokenKind::Op(Operator::Bang),
            TokenKind::Op(Operator::Minus),
        ]) {
            let op_location = op_token.location.clone();
            let op = op_token.to_operator();

            let right = self.unary()?;
            let location = op_location.combine(&right.location);

            return Ok(Expr {
                location,
                kind: ExprKind::Unary(op, Box::new(right)),
            });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.advance_if_match(TokenKind::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.advance_if_match(TokenKind::LeftBracket) {
                expr = self.finish_index(expr)?;
            } else if self.advance_if_match(TokenKind::Op(Operator::Dot)) {
                let name = self.consume_identifier()?;

                expr = Expr {
                    location: self.peek_previous().unwrap().location.clone(),
                    kind: ExprKind::PropertyAccess(Box::new(expr), name),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments = vec![];

        if !self.check(&[TokenKind::RightParen]) {
            loop {
                arguments.push(self.expression()?);

                if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                    break;
                }
            }
        }

        if arguments.len() > MAX_ARGUMENTS {
            self.report(self.error(ParseErrorKind::MaxArgumentsExceeded));
        }

        let t = self.consume_expected(&[TokenKind::RightParen])?.unwrap();

        Ok(Expr {
            location: t.location.clone(),
            kind: ExprKind::Call(Box::new(callee), arguments),
        })
    }

    fn finish_index(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let index = Box::new(self.expression()?);

        let t = self.consume_expected(&[TokenKind::RightBracket])?.unwrap();

        Ok(Expr {
            location: t.location.clone(),
            kind: ExprKind::Index(Box::new(callee), index),
        })
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let next_token = match self.peek() {
            Some(t) => t,
            None => return Err(self.error(ParseErrorKind::Ice("Unexpected end of Token stream"))),
        };

        let location = next_token.location.clone();

        if next_token.is_object() {
            let literal = Object::from_token(next_token).unwrap();

            self.advance();

            Ok(Expr {
                location,
                kind: ExprKind::Literal(literal),
            })
        } else {
            match next_token.kind {
                TokenKind::LeftParen => {
                    self.advance();

                    let expr = self.expression()?;

                    self.consume_expected(&[TokenKind::RightParen])?;

                    Ok(Expr {
                        location,
                        kind: ExprKind::Grouping(Box::new(expr)),
                    })
                }
                TokenKind::LeftBracket => {
                    self.advance();

                    let mut exprs = vec![];
                    while !self.check(&[TokenKind::RightBracket]) && !self.is_at_end() {
                        exprs.push(self.expression()?);

                        if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                            break;
                        }
                    }

                    if self.advance_if_match(TokenKind::Op(Operator::Semicolon)) {
                        let multiplier = self.expression()?;

                        if exprs.len() == 1 {
                            self.consume_expected(&[TokenKind::RightBracket])?;

                            return Ok(Expr {
                                location,
                                kind: ExprKind::ArrayConstructorMulti(
                                    Box::new(exprs.pop().unwrap()),
                                    Box::new(multiplier),
                                ),
                            });
                        } else {
                            return Err(self.error(ParseErrorKind::InvalidConstructor));
                        }
                    }

                    self.consume_expected(&[TokenKind::RightBracket])?;

                    Ok(Expr {
                        location,
                        kind: ExprKind::ArrayConstructor(exprs),
                    })
                }
                TokenKind::Reserved(ReservedWord::Fun) => {
                    self.advance();

                    let function = self.function()?;

                    Ok(Expr {
                        location,
                        kind: ExprKind::Literal(Object::LoxFunc(function)),
                    })
                }
                TokenKind::Reserved(ReservedWord::Super) => {
                    self.advance();

                    self.consume_expected(&[TokenKind::Op(Operator::Dot)])?;

                    let method_name = self.consume_identifier()?;

                    Ok(Expr {
                        location,
                        kind: ExprKind::Super(method_name),
                    })
                }
                TokenKind::Reserved(ReservedWord::This) => {
                    self.advance();

                    Ok(Expr {
                        location,
                        kind: ExprKind::This,
                    })
                }
                TokenKind::Identifier(sym) => {
                    self.advance();

                    Ok(Expr {
                        location,
                        kind: ExprKind::VariableAccess(sym),
                    })
                }
                _ => Err(self.error(ParseErrorKind::UnexpectedToken(next_token.clone()))),
            }
        }
    }
}

impl Iterator for Parser {
    type Item = Result<Stmt, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

/*
expression      →   equality ;
equality        →   comparison ( ( "!=" | "==" ) comparison )* ;
comparison      →   addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition        →   multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication  →   unary ( ( "/" | "*" ) unary )* ;
unary           →   ( "!" | "-" ) unary  | primary ;
primary         →   NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" ;
*/
