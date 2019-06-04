use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::object::Object;
use crate::result::{Error, Result};
use crate::scanner::{Operator, ReservedWord, Token, TokenKind};
use crate::INTERNER;

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn is_at_end(&self) -> bool {
        return self.cursor >= self.tokens.len();
    }

    fn get_token(&self, c: usize) -> Option<&Token> {
        if c < self.tokens.len() {
            Some(&self.tokens[c])
        } else {
            None
        }
    }

    fn peek(&self) -> Option<&Token> {
        let c = self.cursor;
        self.get_token(c)
    }

    fn advance(&mut self) -> Option<&Token> {
        self.cursor += 1;
        let c = self.cursor;
        self.get_token(c)
    }

    fn check(&self, kinds: &[TokenKind]) -> bool {
        match self.peek() {
            Some(t) => {
                if t.in_kinds(kinds) {
                    true
                } else {
                    false
                }
            }
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

    fn peek_previous(&self) -> Option<&Token> {
        if self.cursor == 0 {
            return None;
        }; // avoid underflow
        let c = self.cursor - 1;
        self.get_token(c)
    }

    fn get_operator(&self) -> Operator {
        self.peek_previous().unwrap().to_operator()
    }

    fn consume(&mut self, kinds: &[TokenKind], err: Error) -> Result<Option<&Token>> {
        if self.check(kinds) {
            Ok(self.advance())
        } else {
            Err(err)
        }
    }

    // TODO: implement panic button and synchronize
    fn synchronize(&mut self) -> () {
        self.advance();

        while let Some(token) = self.peek() {
            if let TokenKind::Op(Operator::Semicolon) = self.peek_previous().unwrap().kind() {
                return;
            }

            use ReservedWord::*;
            match token.kind() {
                TokenKind::Reserved(keyword) => match keyword {
                    Class | Fun | Var | For | If | While | Print | Return => return,
                    _ => (),
                },
                _ => (),
            }

            self.advance();
        }
    }

    // Recursive Descent
    // the following functions each represent one rule of the language's grammar

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = vec![];
        while !self.is_at_end() {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt?);
            }
        }
        Ok(statements)
    }

    fn declaration_b(&mut self) -> Result<Stmt> {
        let v = self.variable_declaration()?;
        if let Some(_) = self.check_advance(&[TokenKind::Reserved(ReservedWord::Var)]) {
            return Ok(v);
        }
        self.statement()
    }

    fn declaration(&mut self) -> Option<Result<Stmt>> {
        match self.declaration_b() {
            Ok(v) => {
                Some(Ok(v))
            }
            Err(e) => {
                self.synchronize();
                //Some(Err(Error::Unimplemented("Declaration synchronize hit")))
                Some(Err(e))
            }
        }
    }

    fn variable_declaration(&mut self) -> Result<Stmt> {
        let sym = self.advance().and_then(|t| match t.kind() {
            TokenKind::Identifier(sym) => Some(sym.clone()),
            _ => None,
        }).ok_or(Error::ExpectedIdentifier)?;
        let initializer = {if let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Equal),
        ]) {
            Some(self.expression()?)
        } else {
            None
        }};
        self.consume(
            &[TokenKind::Op(Operator::Semicolon)],
            Error::ExpectedSemicolon, // HERE: errors here even tho there's a semicolon
        )?;
        Ok(Stmt::new(StmtKind::VariableDeclaration(sym, initializer)))
    }

    /*
    program     → declaration* EOF ;

    declaration → varDecl
                | statement ;

    statement   → exprStmt
                | printStmt ;
                */

    fn statement(&mut self) -> Result<Stmt> {
        if let Some(_) = self.check_advance(&[TokenKind::Reserved(ReservedWord::Print)]) {
            return self.print_statement();
        }
        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(
            &[TokenKind::Op(Operator::Semicolon)],
            Error::ExpectedSemicolon,
        )?;
        Ok(Stmt::new(StmtKind::Print(expr)))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(
            &[TokenKind::Op(Operator::Semicolon)],
            Error::ExpectedSemicolon,
        )?;
        Ok(Stmt::new(StmtKind::Expr(expr)))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::EqualEqual),
            TokenKind::Op(Operator::BangEqual),
        ]) {
            let location = expr.location.clone();
            let op = self.get_operator();
            let right = self.comparison()?;
            expr = Expr {
                location,
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.addition()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Greater),
            TokenKind::Op(Operator::GreaterEqual),
            TokenKind::Op(Operator::Less),
            TokenKind::Op(Operator::LessEqual),
        ]) {
            let location = expr.location.clone();
            let op = self.get_operator();
            let right = self.addition()?;
            expr = Expr {
                location,
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr> {
        let mut expr = self.multiplication()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Minus),
            TokenKind::Op(Operator::Plus),
        ]) {
            let location = expr.location.clone();
            let op = self.get_operator();
            let right = self.multiplication()?;
            expr = Expr {
                location,
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Slash),
            TokenKind::Op(Operator::Star),
        ]) {
            let location = expr.location.clone();
            let op = self.get_operator();
            let right = self.unary()?;
            expr = Expr {
                location,
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Bang),
            TokenKind::Op(Operator::Minus),
        ]) {
            let op = self.get_operator();
            let right = self.unary()?;
            let location = right.location.clone();
            return Ok(Expr {
                location,
                kind: ExprKind::Unary(op, Box::new(right)),
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        let t = self.peek();
        if t.is_none() {
            return Err(Error::Ice("Unexpected end of Token stream"));
        }
        let t = t.unwrap();
        let location = t.location().clone();
        Ok(if let Some(literal) = Object::new_from_token(t) {
            self.advance();
            Expr {
                location,
                kind: ExprKind::Literal(literal),
            }
        } else {
            match t.kind() {
                TokenKind::LeftParen => {
                    self.advance();
                    let expr = self.expression()?;
                    self.consume(&[TokenKind::RightParen], Error::UnclosedParenthesis)?;
                    Expr {
                        location,
                        kind: ExprKind::Grouping(Box::new(expr)),
                    }
                }
                _ => return Err(Error::Unimplemented("Unexpected Token")),
            }
        })
    }
}

/*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "false" | "true" | "nil"
               | "(" expression ")" ;
*/
