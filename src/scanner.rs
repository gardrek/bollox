use crate::result::Error;
use crate::result::Result;
use crate::INTERNER;
use crate::SOURCE;
use std::sync::RwLockReadGuard;
use string_interner::Sym;

type SourceReadGuard<'a> = RwLockReadGuard<'a, String>;

use std::fmt;

#[derive(Debug)]
pub struct Token {
    location: SourceLocation,
    kind: TokenKind,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token {:?} at {}, length {}",
            self.kind, self.location.offset, self.location.length,
        )
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    // Single-character symbols.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Op(Operator),

    // Literals
    Number(f64),
    StaticString(Sym),
    UnfinishedString,
    Identifier(Sym),

    Reserved(ReservedWord),
}

impl TokenKind {
    pub fn same_kind(&self, other: &Self) -> bool {
        use TokenKind::*;
        match (self, other) {
            (LeftParen, LeftParen) |
            (RightParen, RightParen) |
            (LeftBrace, LeftBrace) |
            (RightBrace, RightBrace) |
            (Number(_), Number(_)) |
            (StaticString(_), StaticString(_)) |
            (UnfinishedString, UnfinishedString) |
            (Identifier(_), Identifier(_)) => true,
            (Reserved(word_a), Reserved(word_b)) => word_a == word_b,
            (Op(op_a), Op(op_b)) => op_a == op_b,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self,)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReservedWord {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Break,
}

impl Token {
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn location(&self) -> &SourceLocation {
        &self.location
    }

    pub fn in_kinds(&self, kinds: &[TokenKind]) -> bool {
        for k in kinds {
            if self.kind.same_kind(k) {
                return true;
            }
        }
        false
    }

    pub fn to_operator(&self) -> Operator {
        match self.kind() {
            TokenKind::Op(op) => op.clone(),
            _ => panic!("called to_operator on non-operator token"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    // TODO: need some way to get which file a SourceLocation refers to
    offset: usize,
    length: usize,
}

impl SourceLocation {
    fn get_slice<'a>(&self, source: &'a SourceReadGuard) -> &'a str {
        let offset = self.offset;
        &source[offset..(offset + self.length)]
    }

    /*
    fn file_path<'a>(&self) -> &'a str {
        unimplemented!()
    }

    fn line_number(&self) -> usize {
        unimplemented!()
    }

    fn column(&self) -> usize {
        unimplemented!()
    }
    */
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{};{}",
            self.offset,
            self.length,
            /*
            "  --> {} {}:{}"
            self.file_path(),
            self.line_number(),
            self.column(),
            */
        )
    }
}

fn reserved_word(s: &str) -> Option<ReservedWord> {
    use ReservedWord::*;
    Some(match s {
        "and" => And,
        "class" => Class,
        "else" => Else,
        "false" => False,
        "fun" => Fun,
        "for" => For,
        "if" => If,
        "nil" => Nil,
        "or" => Or,
        "print" => Print,
        "return" => Return,
        "super" => Super,
        "this" => This,
        "true" => True,
        "var" => Var,
        "while" => While,
        "break" => Break,
        _ => return None,
    })
}

fn is_alphanumeric(c: char) -> bool {
    c.is_digit(36) || c == '_'
}

//use std::path::PathBuf;

pub struct Scanner {
    //// location of file we're scanning
    //path: PathBuf,
    // location, in bytes which we re currently looking at
    cursor: usize,
    // whether eof was reached
    pub eof: bool,
}

impl Scanner {
    pub fn new() -> Self {
        Self {
            cursor: 0,
            eof: false,
        }
    }

    fn _advance_char<'a>(&mut self) -> Option<char> {
        /*
        let c = self.cursor;

        if c < store.len(self.source_id) {
            let ch = store
                .get_slice(self.source_id, c, 1)
                .chars()
                .next()
                .unwrap();
            self.cursor += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
        */
        unimplemented!()
    }

    fn advance_byte(&mut self, source: &SourceReadGuard) -> Option<u8> {
        let c = self.cursor;
        if c < source.len() {
            self.cursor += 1;
            Some(source[c..(c + 1)].as_bytes()[0])
        } else {
            None
        }
    }

    /*
    fn advance_matching_byte<'a>(&mut self, store: &'a SourceStore, m: u8) -> bool {
        let c = self.cursor;
        if c < store.len(self.source_id) {
            let ch = store.get_slice(self.source_id, c, 1).as_bytes()[0];
            if ch == m {
                self.cursor += 1;
                true
            } else {
                false
            }
        } else {
            false
        }
    }
    */

    fn lookahead<'a>(&self, source: &'a SourceReadGuard, length: usize) -> Option<&'a str> {
        let c = self.cursor;
        if (c + length) < source.len() {
            Some(&source[c..(c + length)])
        } else {
            None
        }
    }

    fn peek_byte(&self, source: &SourceReadGuard) -> Option<u8> {
        let c = self.lookahead(source, 1);
        match c {
            None => None,
            Some(x) => Some(x.as_bytes()[0]),
        }
    }

    fn static_token(&mut self, kind: TokenKind) -> Token {
        let location = SourceLocation {
            offset: self.cursor - 1,
            length: 1,
        };
        Token { location, kind }
    }

    fn static_token_length(&mut self, kind: TokenKind, length: usize) -> Token {
        let location = SourceLocation {
            offset: self.cursor - length,
            length,
        };
        Token { location, kind }
    }

    fn match_static_token(
        &mut self,
        source: &SourceReadGuard,
        m: u8,
        a: TokenKind,
        b: TokenKind,
    ) -> Result<Token> {
        let s = self.lookahead(source, 1);
        if let Some(s) = s {
            Ok(if s.as_bytes()[0] == m {
                self.cursor += 1;
                self.static_token_length(a, 2)
            } else {
                self.static_token_length(b, 1)
            })
        } else {
            Err(Error::Unimplemented(
                "match_static_token unexpected end of stream",
            ))
        }
    }

    fn match_static_operator(
        &mut self,
        source: &SourceReadGuard,
        m: u8,
        a: Operator,
        b: Operator,
    ) -> Result<Token> {
        self.match_static_token(source, m, TokenKind::Op(a), TokenKind::Op(b))
    }

    fn line_comment(&mut self, source: &SourceReadGuard) -> () {
        loop {
            let c = self.advance_byte(source);
            let c = match c {
                None => return,
                Some(x) => x,
            };
            if c == b'\n' {
                self.cursor -= 1;
                return;
            }
        }
    }

    fn number(&mut self, source: &SourceReadGuard) -> Token {
        let mut length = 0;
        let offset = self.cursor;

        let mut decimal = false;
        loop {
            let c = self.peek_byte(source);
            if let Some(c) = c {
                if let Some(_digit) = (c as char).to_digit(10) {
                    self.advance_byte(source);
                    length += 1;
                } else {
                    if c == b'.' {
                        if decimal {
                            break;
                        }
                        self.cursor += 1;
                        let c = self.peek_byte(source);
                        if let Some(c) = c {
                            if (c as char).is_digit(10) {
                                length += 1;
                                decimal = true;
                            } else {
                                self.cursor -= 1;
                                break;
                            }
                        } else {
                            self.cursor -= 1;
                            break;
                        }
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        let location = SourceLocation { offset, length };

        let value = location.get_slice(source).parse::<f64>().ok().unwrap();

        Token {
            location,
            kind: TokenKind::Number(value),
        }
    }

    /* TODO: add escape sequence(s) at least for double quote */
    fn string(&mut self, source: &SourceReadGuard) -> Token {
        let offset = self.cursor - 1;
        let mut length = 0;

        loop {
            let c = self.peek_byte(source);
            if let Some(c) = c {
                self.advance_byte(source);
                length += 1;
                if c == b'"' {
                    length += 1;
                    break;
                }
            } else {
                length += 1;
                let location = SourceLocation { offset, length };
                let token = Token {
                    location,
                    kind: TokenKind::UnfinishedString,
                };
                return token;
            }
        }

        let location = SourceLocation { offset, length };

        /*  Even though we're not multi-threaded yet,
            there's no reason to keep the lock longer than necessary
        */
        let sym = {
            let s = &location.get_slice(source)[1..length - 1];
            let mut interner = INTERNER.write().unwrap();
            interner.get_or_intern(s)
        };

        Token {
            location: location.clone(),
            kind: TokenKind::StaticString(sym),
        }
    }

    fn identifier(&mut self, source: &SourceReadGuard) -> Token {
        let offset = self.cursor;
        let mut length = 0;

        loop {
            let c = self.peek_byte(source);
            if let Some(c) = c {
                if is_alphanumeric(c as char) {
                    self.advance_byte(source);
                    length += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let location = SourceLocation { offset, length };

        let kind = if let Some(word) = reserved_word(location.get_slice(source)) {
            TokenKind::Reserved(word)
        } else {
            let sym = {
                let s = location.get_slice(source);
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern(s)
            };
            TokenKind::Identifier(sym)
        };

        Token { location, kind }
    }

    pub fn collect_or_first_error(&mut self) -> Result<Vec<Token>> {
        let mut v = vec![];
        loop {
            match self.scan_token()? {
                Some(token) => v.push(token),
                None => break,
            }
        }
        Ok(v)
    }

    pub fn scan_token(&mut self) -> Result<Option<Token>> {
        // TODO:  a big one; need to implement rewinding or some other solution to allow parsing
        // a token that is across multiple lines in interactive mode. need some way to signal EOF

        let current = self.cursor;

        let source = SOURCE.read().unwrap();

        let total_length = source.len();

        // If we are past the end
        if current >= total_length {
            self.eof = true;
            return Ok(None);
        }

        let token = loop {
            let first = self.advance_byte(&source);

            // If there's no bytes left return None to signal the end
            let inner = match first {
                Some(byte) => byte,
                None => {
                    self.eof = true;
                    return Ok(None);
                }
            };

            use Operator::*;
            use TokenKind::*;
            match inner {
                b'(' => break self.static_token(LeftParen),
                b')' => break self.static_token(RightParen),
                b'{' => break self.static_token(LeftBrace),
                b'}' => break self.static_token(RightBrace),
                b',' => break self.static_token(Op(Comma)),
                b'.' => break self.static_token(Op(Dot)),
                b'-' => break self.static_token(Op(Minus)),
                b'+' => break self.static_token(Op(Plus)),
                b';' => break self.static_token(Op(Semicolon)),
                b'*' => break self.static_token(Op(Star)),
                b'/' => match self.peek_byte(&source) {
                    // no bytes left
                    None => {
                        self.eof = true;
                        return Ok(None);
                    }
                    Some(c) => match c {
                        b'/' => {
                            self.line_comment(&source);
                            continue;
                        }
                        b'*' => return Err(Error::Unimplemented("Block comments")), // TODO: add block comments here
                        _ => break self.static_token(Op(Slash)),
                    },
                },
                b'!' => break self.match_static_operator(&source, b'=', BangEqual, Bang)?,
                b'=' => break self.match_static_operator(&source, b'=', EqualEqual, Equal)?,
                b'>' => break self.match_static_operator(&source, b'=', GreaterEqual, Greater)?,
                b'<' => break self.match_static_operator(&source, b'=', LessEqual, Less)?,
                b'"' => break self.string(&source),
                c if (c as char).is_whitespace() => {
                    continue;
                }
                c if (c as char).is_digit(10) => {
                    self.cursor -= 1; // FIXME: maybe should do this another way?
                    break self.number(&source);
                }
                c if is_alphanumeric(c as char) => {
                    self.cursor -= 1;
                    break self.identifier(&source);
                }
                // FIXME: This currently treats unrecognized bytes (not chars) as an error.
                // Perhaps we can push this on to the next phase with some small Unicode awareness.
                // That would be the first step to having Unicode in the language.
                // (well second, after refactoring this loop to use chars instead of bytes)
                _ => return Err(Error::Unimplemented("Unrecognized Character Reached")),
            }
        };

        Ok(Some(token))
    }
}

impl Iterator for Scanner {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        //self.scan_token().transpose()
        unimplemented!()
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
