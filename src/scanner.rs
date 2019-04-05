#[derive(Debug, Default)]
pub struct Token<'a> {
    raw: &'a str,
    kind: TokenKind,
    location: SourceLocation<'a>,
}

/*
impl Token<'static> {
    fn new() -> Self {
        Token {
            raw: "",
            kind: TokenKind::Empty,
            location: Default::default(),
        }
    }
}
*/

#[derive(Debug)]
enum TokenKind {
    Empty,
    //Literal(Literal),
    //Eof,
}

impl std::default::Default for TokenKind {
    fn default() -> Self {
        TokenKind::Empty
    }
}

/*
#[derive(Clone, Copy, Debug)]
enum Literal {
    Identifier,
    String(StringLiteralIndex),
    Number(f64),
}

#[derive(Clone, Copy, Debug)]
struct StringLiteralIndex;
*/

#[derive(Debug, Default)]
pub struct SourceLocation<'a> {
    file_path: &'a str,
    line_number: usize,
    column: usize,
}

impl<'a> SourceLocation<'a> {
    //fn new()
}

use std::fmt;
impl<'a> fmt::Display for SourceLocation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "  --> {} {}:{}",
            self.file_path, self.line_number, self.column
        )
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    char_indices: std::str::CharIndices<'a>,
    cursor: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            char_indices: source.char_indices(),
            cursor: 0,
        }
    }

    fn lookahead(&mut self, n: usize) -> &'a str {
        &self.source[self.cursor..(self.cursor + n)]
    }

    fn scan_token(&mut self) -> Option<Token<'a>> {
        None
    }

    pub fn tokens(&self) -> Vec<Token> {
        let mut v = vec![];
        v.resize_with(5, Default::default);
        v
    }
}

impl<'a> Iterator for &mut Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

/*
enum LoxTokenKind {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier, String, Number,

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof,
}
*/
