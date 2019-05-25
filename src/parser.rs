use crate::ast::{Expr, Literal};
use crate::scanner::{Operator, Scanner, Token, TokenKind};
use crate::store::SourceStore;
use crate::store::Store;
use crate::result::Error;

type ExprResult = Result<Expr, Error>;

pub fn test_run(source: String) {
    let mut store = SourceStore::new();

    // move the source in, no need to allocate again
    let id = store.add_from_source(source);

    store.set_eof(id);

    let mut id_store = Store::new(String::from(""));
    let mut string_store = Store::new(String::from(""));

    let mut sc = Scanner::new(id);
    let mut tokens = sc.collect_tokens(&store);

    for token in tokens.iter_mut() {
        eprintln!("{}: {}", token, token.get_slice(&store));
        //eprint!("{} ", token.get_slice(&store));
        token.intern_identifier(&store, &mut id_store);
        token.intern_string(&store, &mut string_store);
    }

    let mut parser = Parser::new(tokens);
    let expr = parser.expression();

    eprintln!("\n{:?}\n", expr);

    //store.remove(id);
}

struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
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

    fn consume(&mut self, kinds: &[TokenKind], err: Error) -> Result<Option<&Token>, Error> {
        if self.check(kinds) {
            Ok(self.advance())
        } else {
            Err(err)
        }
    }

    // Recursive Descent
    // the following functions each represent one rule of the language's grammar

    fn expression(&mut self) -> ExprResult {
        self.equality()
    }

    fn equality(&mut self) -> ExprResult {
        let mut expr = self.comparison()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::EqualEqual),
            TokenKind::Op(Operator::BangEqual),
        ]) {
            let op = self.get_operator();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ExprResult {
        let mut expr = self.addition()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Greater),
            TokenKind::Op(Operator::GreaterEqual),
            TokenKind::Op(Operator::Less),
            TokenKind::Op(Operator::LessEqual),
        ]) {
            let op = self.get_operator();
            let right = self.addition()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ExprResult {
        let mut expr = self.multiplication()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Minus),
            TokenKind::Op(Operator::Plus),
        ]) {
            let op = self.get_operator();
            let right = self.multiplication()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ExprResult {
        let mut expr = self.unary()?;

        while let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Slash),
            TokenKind::Op(Operator::Star),
        ]) {
            let op = self.get_operator();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ExprResult {
        if let Some(_) = self.check_advance(&[
            TokenKind::Op(Operator::Bang),
            TokenKind::Op(Operator::Minus),
        ]) {
            let op = self.get_operator();
            let right = self.unary()?;
            return Ok(Expr::Unary(op, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> ExprResult {
        let t = self.peek();
        if t.is_none() {
            return Err(Error::Unkown);
        }
        let t = t.unwrap();
        Ok(if let Some(literal) = Literal::from_token(t) {
            self.advance();
            Expr::Literal(literal)
        } else {
            match t.kind() {
                TokenKind::LeftParen => {
                    self.advance();
                    let expr = self.expression()?;
                    self.consume(&[TokenKind::RightParen], Error::UnclosedParenthesis)?;
                    Expr::Grouping(Box::new(expr))
                }
                _ => unimplemented!("{:?}", t.kind()),
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
