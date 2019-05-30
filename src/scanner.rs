use crate::result::Error;
use crate::result::Result;

use crate::store::{SourceId, SourceStore, Store, StoreId};

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

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character symbols.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Op(Operator),

    // Literals
    Identifier(StoreId),
    Number(f64),
    StaticString(StoreId),
    UnfinishedString,
    Reserved(ReservedWord),
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
    pub fn _get_slice<'a>(&self, store: &'a SourceStore) -> &'a str {
        self.location.get_slice(store)
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn in_kinds(&self, kinds: &[TokenKind]) -> bool {
        for k in kinds {
            if &self.kind == k {
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

/*
#[derive(Clone, Copy, Debug)]
pub enum Literal {
    Identifier,
    String(StringLiteralIndex),
    Number(f64),
}

#[derive(Clone, Copy, Debug)]
struct StringLiteralIndex;
*/

/*
enum TokenLength {
    Fixed(usize),
    //Max(usize),
    //Min(usize),
    //Range(usize, usize),
    Unbounded,
}

fn token_length(kind: TokenKind) -> TokenLength {

}
*/

#[derive(Debug, Clone)]
pub struct SourceLocation {
    source_id: SourceId,
    //eof: bool,
    offset: usize,
    length: usize,
}

impl SourceLocation {
    fn get_slice<'a>(&self, store: &'a SourceStore) -> &'a str {
        store.get_slice(self.source_id, self.offset, self.length)
    }

    fn file_path<'a>(&self) -> &'a str {
        unimplemented!()
    }

    fn line_number(&self) -> usize {
        unimplemented!()
    }

    fn column(&self) -> usize {
        unimplemented!()
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "  --> {} {}:{}",
            self.file_path(),
            self.line_number(),
            self.column(),
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

pub struct Scanner {
    source_id: SourceId,
    // location, in bytes which we re currently looking at
    cursor: usize,
    // whether eof was reached
    //eof: bool,
}

impl Scanner {
    pub fn new(source_id: SourceId) -> Self {
        Self {
            source_id,
            cursor: 0,
            //eof: false,
        }
    }

    fn _advance_char<'a>(&mut self, store: &'a SourceStore) -> Option<char> {
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
    }

    fn advance_byte<'a>(&mut self, store: &'a SourceStore) -> Option<u8> {
        let c = self.cursor;
        if c < store.len(self.source_id) {
            self.cursor += 1;
            Some(store.get_slice(self.source_id, c, 1).as_bytes()[0])
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

    fn lookahead<'a>(&self, store: &'a SourceStore, length: usize) -> Option<&'a str> {
        let c = self.cursor;
        if (c + length) < store.len(self.source_id) {
            Some(store.get_slice(self.source_id, c, length))
        } else {
            None
        }
    }

    fn peek_byte(&self, store: &SourceStore) -> Option<u8> {
        let c = self.lookahead(store, 1);
        match c {
            None => None,
            Some(x) => Some(x.as_bytes()[0]),
        }
    }

    fn static_token(&mut self, kind: TokenKind) -> Token {
        let location = SourceLocation {
            source_id: self.source_id,
            offset: self.cursor - 1,
            length: 1,
        };
        Token { location, kind }
    }

    fn static_token_length(&mut self, kind: TokenKind, length: usize) -> Token {
        let location = SourceLocation {
            source_id: self.source_id,
            offset: self.cursor - length,
            length,
        };
        Token { location, kind }
    }

    fn match_static_token(
        &mut self,
        store: &SourceStore,
        m: u8,
        a: TokenKind,
        b: TokenKind,
    ) -> Result<Token> {
        let s = self.lookahead(store, 1);
        if let Some(s) = s {
            Ok(if s.as_bytes()[0] == m {
                self.cursor += 1;
                self.static_token_length(a, 2)
            } else {
                self.static_token_length(b, 1)
            })
        } else {
            Err(Error::Unimplemented("match_static_token unexpected end of stream"))
        }
    }

    fn match_static_operator(
        &mut self,
        store: &SourceStore,
        m: u8,
        a: Operator,
        b: Operator,
    ) -> Result<Token> {
        self.match_static_token(store, m, TokenKind::Op(a), TokenKind::Op(b))
    }

    fn line_comment(&mut self, store: &SourceStore) -> () {
        loop {
            let c = self.advance_byte(store);
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

    fn number(&mut self, store: &SourceStore) -> Token {
        let mut length = 0;
        let offset = self.cursor;

        let mut decimal = false;
        loop {
            let c = self.peek_byte(store);
            if let Some(c) = c {
                if let Some(_digit) = (c as char).to_digit(10) {
                    self.advance_byte(store);
                    length += 1;
                } else {
                    if c == b'.' {
                        if decimal {
                            break;
                        }
                        self.cursor += 1;
                        let c = self.peek_byte(store);
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

        let location = SourceLocation {
            source_id: self.source_id,
            offset,
            length,
        };

        let value = location.get_slice(store).parse::<f64>().ok().unwrap();

        Token {
            location,
            kind: TokenKind::Number(value),
        }
    }

    /* TODO: Strings will need to be copied in some fashion for compiling and for
    escape sequences if they are added */
    fn string(&mut self, source_store: &SourceStore, static_store: &mut Store<String>) -> Token {
        let offset = self.cursor - 1;
        let mut length = 0;

        loop {
            let c = self.peek_byte(source_store);
            if let Some(c) = c {
                self.advance_byte(source_store);
                length += 1;
                if c == b'"' {
                    length += 1;
                    break;
                }
            } else {
                length += 1;
                let location = SourceLocation {
                    source_id: self.source_id,
                    offset,
                    length,
                };
                let token = Token {
                    location,
                    kind: TokenKind::UnfinishedString,
                };
                return token;
            }
        }

        let location = SourceLocation {
            source_id: self.source_id,
            offset,
            length,
        };

        let s = &location.get_slice(source_store)[1..length-1];
        let static_store_id = static_store.add(s.into());

        Token {
            location,
            kind: TokenKind::StaticString(static_store_id),
        }
    }

    fn identifier(&mut self, source_store: &SourceStore, static_store: &mut Store<String>) -> Token {
        let offset = self.cursor;
        let mut length = 0;

        loop {
            let c = self.peek_byte(source_store);
            if let Some(c) = c {
                if is_alphanumeric(c as char) {
                    self.advance_byte(source_store);
                    length += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let location = SourceLocation {
            source_id: self.source_id,
            offset,
            length,
        };

        let kind = if let Some(word) = reserved_word(location.get_slice(source_store)) {
            TokenKind::Reserved(word)
        } else {
            let s = location.get_slice(source_store);
            let static_store_id = static_store.add(s.into());
            TokenKind::Identifier(static_store_id)
        };

        Token { location, kind }
    }

    pub fn collect_or_first_error(&mut self, source_store: &SourceStore, mut static_store: &mut Store<String>) -> Result<Vec<Token>> {
        let mut v = vec![];
        loop {
            match self.scan_token(&source_store, &mut static_store)? {
                Some(token) => v.push(token),
                None => break,
            }
        }
        Ok(v)
    }

    pub fn scan_token(&mut self, source_store: &SourceStore, mut static_store: &mut Store<String>) -> Result<Option<Token>> {
        // TODO:  a big one; need to implement rewinding or some other solution to allow parsing
        // a token that is across multiple lines in interactive mode. need some way to signal EOF

        let current = self.cursor;
        let total_length = source_store.len(self.source_id);

        // If we are past the end
        if current >= total_length {
            return Ok(None);
        }

        let token = loop {
            let first = self.advance_byte(source_store);

            // If there's no bytes left return None to signal the end
            let inner = match first {
                    Some(byte) => byte,
                    None => return Ok(None),
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
                b'/' => match self.peek_byte(source_store) {
                    // no bytes left
                    None => return Ok(None),
                    Some(c) => match c {
                        b'/' => {
                            self.line_comment(source_store);
                            continue;
                        }
                        b'*' => return Err(Error::Unimplemented("Block comments")), // TODO: add block comments here
                        _ => break self.static_token(Op(Slash)),
                    },
                },
                b'!' => break self.match_static_operator(source_store, b'=', BangEqual, Bang)?,
                b'=' => break self.match_static_operator(source_store, b'=', EqualEqual, Equal)?,
                b'>' => break self.match_static_operator(source_store, b'=', GreaterEqual, Greater)?,
                b'<' => break self.match_static_operator(source_store, b'=', LessEqual, Less)?,
                b'"' => break self.string(source_store, &mut static_store),
                c if (c as char).is_whitespace() => {
                    continue;
                }
                c if (c as char).is_digit(10) => {
                    self.cursor -= 1; // FIXME: maybe should do this another way?
                    break self.number(source_store);
                }
                c if is_alphanumeric(c as char) => {
                    self.cursor -= 1;
                    break self.identifier(source_store, &mut static_store);
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
        //self.scan_token()
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
