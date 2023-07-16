use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::object::Object;
use crate::source::SourceLocation;
use crate::token::{Operator, ReservedWord, Token, TokenKind};

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    //~ Unimplemented(&'static str),
    ExpectedIdentifier,
    ExpectedSemicolon,
    ExpectedRightBrace,
    Ice(&'static str),
    UnclosedParenthesis,
    UnexpectedToken(Token),
    //~ UnexpectedEof,
}

impl std::error::Error for ParseErrorKind {}

/*
impl From<ParseErrorKind> for crate::result::Error {
    fn from(other: ParseErrorKind) -> crate::result::Error {
        crate::result::Error::Parser(other)
    }
}
*/

impl From<ParseError> for crate::result::Error {
    fn from(other: ParseError) -> crate::result::Error {
        crate::result::Error::Parser(other)
    }
}

/*
impl From<ParseError> for ParseErrorKind {
    fn from(other: ParseError) -> ParseErrorKind {
        other.kind.clone()
    }
}
*/

impl core::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        use ParseErrorKind::*;

        match self {
            //~ Unimplemented(s) => write!(f, "Unimplemented feature: {}", s),
            ExpectedIdentifier => write!(f, "Expected Identifier"),
            ExpectedSemicolon => write!(f, "Expected Semicolon"),
            ExpectedRightBrace => write!(f, "Expected Ending Brace"),
            Ice(s) => write!(f, "Internal Compiler Error: {}", s),
            UnclosedParenthesis => write!(f, "Unclosed Parenthesis"),
            UnexpectedToken(t) => write!(f, "Unexpected Token {:?}", t),
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
    tokens: Vec<Token>,
    cursor: usize,
    //~ errors: Vec<ParseErrorKind>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            //~ errors: vec![],
        }
    }

    fn is_at_end(&self) -> bool {
        self.cursor >= self.tokens.len()
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

    fn peek_previous(&self) -> Option<&Token> {
        if self.cursor == 0 {
            return None;
        }; // avoid underflow
        let c = self.cursor - 1;
        self.get_token(c)
    }

    fn consume(
        &mut self,
        kinds: &[TokenKind],
        err: ParseErrorKind,
    ) -> Result<Option<&Token>, ParseErrorKind> {
        if self.check(kinds) {
            Ok(self.advance())
        } else {
            Err(err)
        }
        //~ match self.check_advance(kinds) {
        //~ Some(v) => Ok(Some(v)),
        //~ None => Err(err),
        //~ }
        //~ self.check_advance(kinds).ok_or(err)
    }

    // TODO: implement panic button and synchronize
    fn _synchronize(&mut self) {
        self.advance();

        while let Some(token) = self.peek() {
            if let TokenKind::Op(Operator::Semicolon) = self.peek_previous().unwrap().kind {
                break;
            }

            use ReservedWord::*;
            if let TokenKind::Reserved(keyword) = &token.kind {
                match keyword {
                    Class | Fun | Var | For | If | While | Print | Return => break,
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
        get_argument: fn(&mut Parser) -> Result<Expr, ParseErrorKind>,
        kinds: &[TokenKind],
    ) -> Result<Expr, ParseErrorKind> {
        let mut expr = get_argument(self)?;
        let mut location = expr.location.clone();

        while self.check_advance(kinds).is_some() {
            let op_token = self.peek_previous().unwrap();
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
    pub fn parse_all(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while !self.is_at_end() {
            if let Some(stmt) = self.parse_next() {
                statements.push(stmt?);
            }
        }
        Ok(statements)
    }

    // equivalent of jlox's parse()
    fn parse_next(&mut self) -> Option<Result<Stmt, ParseError>> {
        if self.is_at_end() {
            return None;
        }

        match self.declaration() {
            Ok(stmt) => Some(Ok(stmt)),
            Err(kind) => {
                //~ self.errors.push(e);
                //~ self.synchronize();
                //~ unimplemented!()
                //~ return None
                let t = match self.peek() {
                    Some(t) => t,
                    None => {
                        let location = SourceLocation::bullshit();
                        return Some(Err(ParseError { location, kind }));
                    }
                };

                let location = t.location.clone();

                let e = ParseError { location, kind };

                Some(Err(e))
            }
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParseErrorKind> {
        let maybe = if self
            .check_advance(&[TokenKind::Reserved(ReservedWord::Var)])
            .is_some()
        {
            self.variable_declaration()
        } else {
            self.statement()
        };
        match maybe {
            Ok(v) => Ok(v),
            Err(e) => {
                // TODO: FIXME: commenting this out has fixed our problem i suppose.
                // so this funciton is where the problem is, maybe?
                //~ self.synchronize();
                //Some(Err(ParseErrorKind::Unimplemented("Declaration synchronize hit")))
                eprintln!("Synchro branch");
                Err(e)
            }
        }
    }

    fn variable_declaration(&mut self) -> Result<Stmt, ParseErrorKind> {
        let sym = self
            .peek()
            .and_then(|t| match &t.kind {
                TokenKind::Identifier(sym) => Some(*sym),
                _ => None,
            })
            .ok_or(ParseErrorKind::ExpectedIdentifier)?;

        self.advance();

        let initializer = if self
            .check_advance(&[TokenKind::Op(Operator::Equal)])
            .is_some()
        {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            &[TokenKind::Op(Operator::Semicolon)],
            ParseErrorKind::ExpectedSemicolon,
        )?;

        Ok(Stmt::new(StmtKind::VariableDeclaration(sym, initializer)))
    }

    fn statement(&mut self) -> Result<Stmt, ParseErrorKind> {
        if self
            .check_advance(&[TokenKind::Reserved(ReservedWord::Print)])
            .is_some()
        {
            return self.print_statement();
        }

        if self
            .check_advance(&[TokenKind::Reserved(ReservedWord::If)])
            .is_some()
        {
            return self.rust_style_if_statement();
        }

        if self.check_advance(&[TokenKind::LeftBrace]).is_some() {
            return Ok(Stmt::new(StmtKind::Block(self.block()?)));
        }

        self.expression_statement()
    }

    fn _c_style_if_statement(&mut self) -> Result<Stmt, ParseErrorKind> {
        self.consume(&[TokenKind::LeftParen], ParseErrorKind::ExpectedSemicolon)?;

        let condition = self.expression()?;

        self.consume(&[TokenKind::RightParen], ParseErrorKind::ExpectedSemicolon)?;

        let then_branch = self.statement()?;

        let else_branch = if self
            .check_advance(&[TokenKind::Reserved(ReservedWord::Else)])
            .is_some()
        {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::new(StmtKind::If(
            condition,
            Box::new(then_branch),
            else_branch,
        )))
    }

    fn rust_style_if_statement(&mut self) -> Result<Stmt, ParseErrorKind> {
        let condition = self.expression()?;

        let then_block = if self.check_advance(&[TokenKind::LeftBrace]).is_some() {
            Stmt::new(StmtKind::Block(self.block()?))
        } else {
            panic!()
        };

        let else_block = if self
            .check_advance(&[TokenKind::Reserved(ReservedWord::Else)])
            .is_some()
        {
            if self.check_advance(&[TokenKind::LeftBrace]).is_some() {
                Some(Box::new(Stmt::new(StmtKind::Block(self.block()?))))
            } else {
                panic!()
            }
        } else {
            None
        };

        Ok(Stmt::new(StmtKind::If(
            condition,
            Box::new(then_block),
            else_block,
        )))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseErrorKind> {
        let expr = self.expression()?;
        self.consume(
            &[TokenKind::Op(Operator::Semicolon)],
            ParseErrorKind::ExpectedSemicolon,
        )?;
        Ok(Stmt::new(StmtKind::Print(expr)))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseErrorKind> {
        let mut stmts = vec![];

        while !self.check(&[TokenKind::RightBrace]) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(&[TokenKind::RightBrace], ParseErrorKind::ExpectedRightBrace)?;

        Ok(stmts)
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseErrorKind> {
        let expr = self.expression()?;
        self.consume(
            &[TokenKind::Op(Operator::Semicolon)],
            ParseErrorKind::ExpectedSemicolon,
        )?;
        Ok(Stmt::new(StmtKind::Expr(expr)))
    }

    fn expression(&mut self) -> Result<Expr, ParseErrorKind> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseErrorKind> {
        let expr = self.logical_or()?;

        // TODO: refactor to use check_advance
        if self
            .check_advance(&[TokenKind::Op(Operator::Equal)])
            .is_some()
        {
            let value = self.assignment();

            return Ok(match expr.kind {
                ExprKind::VariableAccess(name) => Expr {
                    location: expr.location,
                    kind: ExprKind::Assign(name, Box::new(value?)),
                },
                _ => panic!("invalid assignment target"),
            });
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParseErrorKind> {
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

    fn logical_and(&mut self) -> Result<Expr, ParseErrorKind> {
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

    fn equality(&mut self) -> Result<Expr, ParseErrorKind> {
        self.binary_op(
            Parser::comparison,
            &[
                TokenKind::Op(Operator::EqualEqual),
                TokenKind::Op(Operator::BangEqual),
            ],
        )
    }

    fn comparison(&mut self) -> Result<Expr, ParseErrorKind> {
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

    fn addition(&mut self) -> Result<Expr, ParseErrorKind> {
        self.binary_op(
            Parser::multiplication,
            &[
                TokenKind::Op(Operator::Minus),
                TokenKind::Op(Operator::Plus),
            ],
        )
    }

    fn multiplication(&mut self) -> Result<Expr, ParseErrorKind> {
        self.binary_op(
            Parser::unary,
            &[
                TokenKind::Op(Operator::Slash),
                TokenKind::Op(Operator::Star),
            ],
        )
    }

    fn unary(&mut self) -> Result<Expr, ParseErrorKind> {
        if self
            .check_advance(&[
                TokenKind::Op(Operator::Bang),
                TokenKind::Op(Operator::Minus),
            ])
            .is_some()
        {
            let op_token = self.peek_previous().unwrap();
            let mut location = op_token.location.clone();
            let op = op_token.to_operator();

            let right = self.unary()?;
            location = location.combine(&right.location);

            return Ok(Expr {
                location,
                kind: ExprKind::Unary(op, Box::new(right)),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseErrorKind> {
        if self.peek().is_none() {
            return Err(ParseErrorKind::Ice("Unexpected end of Token stream"));
        }

        let next_token = self.peek().unwrap();

        if next_token.is_object() {
            let location = next_token.location.clone();

            let literal = Object::from_token(next_token).unwrap();

            self.advance();

            Ok(Expr {
                location,
                kind: ExprKind::Literal(literal),
            })
        } else {
            let location = next_token.location.clone();

            match next_token.kind {
                TokenKind::LeftParen => {
                    self.advance();

                    let expr = self.expression()?;

                    self.consume(
                        &[TokenKind::RightParen],
                        ParseErrorKind::UnclosedParenthesis,
                    )?;

                    Ok(Expr {
                        location,
                        kind: ExprKind::Grouping(Box::new(expr)),
                    })
                }
                TokenKind::Identifier(sym) => {
                    self.advance();

                    Ok(Expr {
                        location,
                        kind: ExprKind::VariableAccess(sym),
                    })
                }
                _ => Err(ParseErrorKind::UnexpectedToken(next_token.clone())),
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
