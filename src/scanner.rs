//use crate::result::Result;
//use crate::result::Error;

use crate::store::{SourceId, SourceStore, Identifier, IdentifierStore};

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

    pub fn intern_identifier(&mut self, store: &SourceStore, id_store: &mut IdentifierStore) -> Option<Identifier> {
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
                            },
                        }
                    },
                };
                match id {
                    Some(_) => self.kind = TokenKind::Identifier(id),
                    None => unreachable!(),
                }
                id
            },
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum TokenKind {
    // Single-character symbols.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character symbols.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(Option<Identifier>),
    Number(f64),
    String,
    //String(String),
    Reserved(ReservedWord),

    //Eof,

    // TODO: remove this and start using Result<Token>
    SyntaxError,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

    fn static_token(&mut self, kind: TokenKind) -> Option<Token> {
        let location = SourceLocation {
            source_id: self.source_id,
            offset: self.cursor - 1,
            length: 1,
        };
        Some(Token { location, kind })
    }

    fn match_static_token(
        &mut self,
        store: &SourceStore,
        m: u8,
        a: TokenKind,
        b: TokenKind,
    ) -> Option<Token> {
        let s = self.lookahead(store, 1);
        if let Some(s) = s {
            self.static_token(if s.as_bytes()[0] == m { a } else { b })
        } else {
            panic!("match_static_token")
        }
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

    fn number(&mut self, store: &SourceStore) -> Option<Token> {
        let mut length = 0;
        let offset = self.cursor;

        let mut whole = 0;
        let mut fraction = 0f64;
        let mut decimal_digit = 0;
        loop {
            let c = self.peek_byte(store);
            if let Some(c) = c {
                if let Some(digit) = (c as char).to_digit(10) {
                    self.advance_byte(store);
                    length += 1;
                    // TODO: make decimal handling better
                    if decimal_digit > 0 {
                        fraction += 10.0_f64.powf(-decimal_digit as f64) * digit as f64;
                        decimal_digit += 1;
                    } else {
                        whole = whole * 10 + digit;
                    }
                } else {
                    if c == b'.' {
                        self.cursor += 1;
                        let c = self.peek_byte(store);
                        if let Some(c) = c {
                            if (c as char).is_digit(10) {
                                length += 1;
                                decimal_digit = 1;
                            } else {
                                self.cursor -= 1;
                                break;
                            }
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            } else {
                return None;
            }
        }

        let location = SourceLocation {
            source_id: self.source_id,
            offset,
            length,
        };
        Some(Token {
            location,
            kind: TokenKind::Number(whole as f64 + fraction),
        })
    }

    /*  TODO: Strings will need to be copied in some fashion for compiling and for
    escape sequences if they are added */
    fn string(&mut self, store: &SourceStore) -> Option<Token> {
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
                return None;
            }
        }

        let location = SourceLocation {
            source_id: self.source_id,
            offset,
            length,
        };
        Some(Token {
            location,
            kind: TokenKind::String,
        })
    }

    fn identifier(&mut self, store: &SourceStore) -> Option<Token> {
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
                return None;
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

        Some(Token { location, kind })
    }

    pub fn scan_token(&mut self, store: &SourceStore) -> Option<Token> {
        let current = self.cursor;
        let _offset = current;
        let total_length = store.len(self.source_id);

        // If we are past the end
        if current > total_length {
            return None;
        }

        let token;

        if current < total_length {
            if total_length.saturating_sub(current) == 0 {
                panic!();
            } else {
                //let first = store.get_slice(self.source_id, current, 1);
                let err_token = Some(Token {
                    location: SourceLocation {
                        source_id: self.source_id,
                        offset: current,
                        length: 0,
                    },
                    kind: TokenKind::SyntaxError,
                });

                token = loop {
                    let first = self.advance_byte(store);
                    let inner = match first {
                        None => break err_token,
                        Some(x) => x,
                    };
                    use TokenKind::*;
                    match inner {
                        b'(' => break self.static_token(LeftParen),
                        b')' => break self.static_token(RightParen),
                        b'{' => break self.static_token(LeftBrace),
                        b'}' => break self.static_token(RightBrace),
                        b',' => break self.static_token(Comma),
                        b'.' => break self.static_token(Dot),
                        b'-' => break self.static_token(Minus),
                        b'+' => break self.static_token(Plus),
                        b';' => break self.static_token(Semicolon),
                        b'*' => break self.static_token(Star),
                        b'/' => match self.peek_byte(store) {
                            None => break err_token,
                            Some(c) => match c {
                                b'/' => {
                                    self.line_comment(store);
                                    continue;
                                }
                                b'*' => break err_token, // TODO: add block comments here
                                _ => break self.static_token(Slash),
                            },
                        },
                        b'!' => break self.match_static_token(store, b'=', BangEqual, Bang),
                        b'=' => break self.match_static_token(store, b'=', EqualEqual, Equal),
                        b'>' => break self.match_static_token(store, b'=', GreaterEqual, Greater),
                        b'<' => break self.match_static_token(store, b'=', LessEqual, Less),
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
            }
        } else {
            self.cursor += 1; // now we are past the end, signaling to not produce any more tokens
                              //self.eof = true;
            token = None;
        }

        token
    }
}

/*
pub fn test_run(source: String) {
    let mut store = SourceStore::new();
    let id = store.add_empty();
    store.push_str(id, &source);

    let mut sc = Scanner::new(id);
    loop {
        match sc.scan_token(&store) {
            Some(token) => eprintln!("{}: \"{}\"", token, token.location.get_slice(&store)),
            None => break,
        }
    }

    store.remove(id);
}
*/

//impl<'a> Iterator for &mut Scanner<'a> {
//type Item = Token<'a>;

//fn next(&mut self) -> Option<Self::Item> {
//self.scan_token()
//}
//}

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
