//use crate::result::Result;
//use crate::result::Error;

use crate::store::{SourceId, SourceStore, Store, StoreId};

type Identifier = StoreId;

type IdentifierStore = Store<String>;

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

impl Token {
    pub fn get_slice<'a>(&self, store: &'a SourceStore) -> &'a str {
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

    pub fn intern_identifier(
        &mut self,
        store: &SourceStore,
        id_store: &mut IdentifierStore,
    ) -> Option<Identifier> {
        match self.kind {
            TokenKind::Identifier(option_id) => {
                let id = match option_id {
                    Some(_) => return None,
                    None => {
                        let string = String::from(self.get_slice(store));
                        match id_store.get_id(&string) {
                            Some(&id) => Some(id),
                            None => {
                                let id = id_store.add(string);
                                Some(id)
                            }
                        }
                    }
                };
                match id {
                    Some(_) => self.kind = TokenKind::Identifier(id),
                    None => unreachable!(),
                }
                id
            }
            _ => None,
        }
    }

    pub fn intern_string(
        &mut self,
        store: &SourceStore,
        id_store: &mut Store<String>,
    ) -> Option<StoreId> {
        match self.kind {
            TokenKind::StaticString(option_id) => {
                let id = match option_id {
                    Some(_) => return None,
                    None => {
                        let string = String::from(self.get_slice(store));
                        match id_store.get_id(&string) {
                            Some(&id) => Some(id),
                            None => {
                                let id = id_store.add(string);
                                Some(id)
                            }
                        }
                    }
                };
                match id {
                    Some(_) => self.kind = TokenKind::StaticString(id),
                    None => unreachable!(),
                }
                id
            }
            _ => None,
        }
    }

    pub fn _intern_any(
        &mut self,
        store: &SourceStore,
        intern_store: &mut Store<String>,
    ) -> Option<StoreId> {
        match self.kind {
            TokenKind::Identifier(_) => self.intern_identifier(store, intern_store),
            TokenKind::StaticString(_) => self.intern_string(store, intern_store),
            _ => None,
        }
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
    Identifier(Option<Identifier>),
    Number(f64),
    StaticString(Option<StoreId>),
    UnfinishedString,
    //String(String),
    Reserved(ReservedWord),

    Eof,
    // TODO: start using Result<Token>
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

    /*
    fn advance_char<'a>(&mut self, store: &'a SourceStore) -> Option<char> {
        if self.eof {
            return None;
        }

        let c = self.cursor;
        if c < store.len(self.source_id) {
            self.cursor += 1;
            Some(store.get_slice(self.source_id, c, 1).as_bytes()[0])
        }

        unimplemented!()
    }
    */

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
    ) -> Token {
        let s = self.lookahead(store, 1);
        if let Some(s) = s {
            if s.as_bytes()[0] == m {
                self.cursor += 1;
                self.static_token_length(a, 2)
            } else {
                self.static_token_length(b, 1)
            }
        } else {
            panic!("match_static_token")
        }
    }

    fn match_static_operator(
        &mut self,
        store: &SourceStore,
        m: u8,
        a: Operator,
        b: Operator,
    ) -> Token {
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

    /*  TODO: Strings will need to be copied in some fashion for compiling and for
    escape sequences if they are added */
    fn string(&mut self, store: &SourceStore) -> Token {
        let offset = self.cursor - 1;
        let mut length = 0;

        loop {
            let c = self.peek_byte(store);
            if let Some(c) = c {
                self.advance_byte(store);
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
        Token {
            location,
            kind: TokenKind::StaticString(None),
        }
    }

    fn identifier(&mut self, store: &SourceStore) -> Token {
        let offset = self.cursor;
        let mut length = 0;

        loop {
            let c = self.peek_byte(store);
            if let Some(c) = c {
                if is_alphanumeric(c as char) {
                    self.advance_byte(store);
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

        let kind = if let Some(word) = reserved_word(location.get_slice(store)) {
            TokenKind::Reserved(word)
        } else {
            TokenKind::Identifier(None)
        };

        Token { location, kind }
    }

    pub fn collect_tokens(&mut self, store: &SourceStore) -> Vec<Token> {
        let mut v = vec![];
        loop {
            match self.scan_token(store) {
                None => break,
                Some(t) => v.push(t),
            }
        }
        v
    }

    pub fn scan_token(&mut self, store: &SourceStore) -> Option<Token> {
        let current = self.cursor;
        let total_length = store.len(self.source_id);

        // If we are past the end
        if current > total_length {
            return None;
        }

        let token;

        if current < total_length {
            //let first = store.get_slice(self.source_id, current, 1);
            let err_token = Token {
                location: SourceLocation {
                    source_id: self.source_id,
                    offset: current,
                    length: 0,
                },
                kind: TokenKind::Eof,
            };

            token = loop {
                let first = self.advance_byte(store);
                let inner = match first {
                    None => break err_token,
                    Some(x) => x,
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
                    b'/' => match self.peek_byte(store) {
                        None => break err_token,
                        Some(c) => match c {
                            b'/' => {
                                self.line_comment(store);
                                continue;
                            }
                            b'*' => break err_token, // TODO: add block comments here
                            _ => break self.static_token(Op(Slash)),
                        },
                    },
                    b'!' => break self.match_static_operator(store, b'=', BangEqual, Bang),
                    b'=' => break self.match_static_operator(store, b'=', EqualEqual, Equal),
                    b'>' => {
                        break self.match_static_operator(store, b'=', GreaterEqual, Greater)
                    }
                    b'<' => break self.match_static_operator(store, b'=', LessEqual, Less),
                    b'"' => break self.string(store),
                    c if (c as char).is_whitespace() => {
                        //self.cursor += 1;
                        continue;
                    }
                    c if (c as char).is_digit(10) => {
                        self.cursor -= 1; // FIXME: maybe should do this another way?
                        break self.number(store);
                    }
                    c if is_alphanumeric(c as char) => {
                        self.cursor -= 1;
                        break self.identifier(store);
                    }
                    _ => break err_token,
                }
            };
        } else {
            self.cursor += 1; // now we are past the end, signaling to not produce any more tokens
            return None
            /*
            //self.eof = true;
            token = Token {
                location: SourceLocation {
                    source_id: self.source_id,
                    offset: current,
                    length: 0,
                },
                kind: TokenKind::Eof,
            };
            */
        }

        Some(token)
    }
}

/*
impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        //self.scan_token()
        unimplemented!()
    }
}
*/

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
