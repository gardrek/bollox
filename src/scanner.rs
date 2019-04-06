use std::fmt;
use crate::result::Result;

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
            self.kind,
            self.location.offset,
            self.location.length,
        )
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    // Single-character symbols.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character symbols.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    /* 
    // Literals.
    Identifier, String, Number,

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    */

    //Literal(Literal),
    Eof,

    //TODO: remove this and start using Result<Token>
    SyntaxError,
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



#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct SourceId(usize);

#[derive(Debug)]
pub struct SourceLocation {
    source_id: SourceId,
    offset: usize,
    length: usize,
}

impl SourceLocation {
    /*fn get_slice<'a>(&self, source: &'a str) -> &'a str {
        let n = self.offset;
        &source[n..(n + self.length)]
    }*/

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

pub struct Scanner {
    source_id: SourceId,
    cursor: usize,
}

impl Scanner {
    pub fn new(source_id: SourceId) -> Self {
        Self {
            source_id,
            cursor: 0,
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

    fn lookahead<'a>(&mut self, store: &'a SourceStore, length: usize) -> Option<&'a str> {
        let c = self.cursor;
        if (c + length) < store.len(self.source_id) {
            Some(store.get_slice(self.source_id, c, length))
        } else {
            None
        }
    }

    pub fn scan_token(&mut self, store: &SourceStore) -> Option<Token> {
        let current = self.cursor;
        let offset = current;
        let total_length = store.len(self.source_id);

        // If we are past the end
        if current > total_length {
            return None;
        }

        let length;
        let kind;

        if current < total_length {
            if total_length.saturating_sub(current) == 0 {
                panic!();
            } else {
                //let first = store.get_slice(self.source_id, current, 1);
                let first = self.advance_byte(store).unwrap();

                {
                    use TokenKind::*;
                    kind = match first {
                        b'(' => LeftParen,
                        b')' => RightParen,
                        b'{' => LeftBrace,
                        b'}' => RightBrace,
                        b',' => Comma,
                        b'.' => Dot,
                        b'-' => Minus,
                        b'+' => Plus,
                        b';' => Semicolon,
                        b'*' => Star,
                        b'/' => {
                            unimplemented!()
                            /*
                            loop {
                                match self.lookahead(store, 1) {
                                    None => 
                                if x == "\n" {
                                    break
                                }
                            }
                            */
                        }
                        b'!' => {
                            if self.advance_matching_byte(store, b'=') {
                                BangEqual
                            } else {
                                Bang
                            }
                        }
                        b'=' => {
                            if self.advance_matching_byte(store, b'=') {
                                EqualEqual
                            } else {
                                Equal
                            }
                        }
                        b'<' => {
                            if self.advance_matching_byte(store, b'=') {
                                LessEqual
                            } else {
                                Less
                            }
                        }
                        b'>' => {
                            if self.advance_matching_byte(store, b'=') {
                                GreaterEqual
                            } else {
                                Greater
                            }
                        }
                        _ => SyntaxError,
                    };
                }

                length = 1;
            }
        } else {
            self.cursor += 1; // now we are past the end, signaling to not produce aymore tokens
            length = 0;
            kind = TokenKind::Eof;
        }

        let location = SourceLocation {
            source_id: self.source_id,
            offset,
            length,
        };
        Some(Token {
            location,
            kind,
        })
    }
}

pub fn test_run(source: String) {
    let mut store = SourceStore::new();
    let id = store.add_id().unwrap();
    store.push_str(id, &source);

    let mut sc = Scanner::new(id);
    loop {
        match sc.scan_token(&store) {
            Some(token) => eprintln!("{}", token),
            None => break,
        }
    }
}

use std::collections::HashMap;

pub struct SourceStore {
    data: HashMap<SourceId, String>,
    next_id: usize,
}

impl SourceStore {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            next_id: 1,
        }
    }

    pub fn add_id(&mut self) -> Option<SourceId> {
        let id = SourceId(self.next_id);
        if self.data.contains_key(&id) {
            return None;
        }
        self.data.insert(id, String::new());
        self.next_id += 1;
        Some(id)
    }

    pub fn push_str(&mut self, id: SourceId, s: &str) {
        if self.data.contains_key(&id) {
            match self.data.get_mut(&id) {
                Some(inner) => inner.push_str(s),
                None => panic!(),
            }
        } else {
            panic!()
        }
    }

    pub fn get_slice(&self, id: SourceId, start: usize, length: usize) -> &str {
        if self.data.contains_key(&id) {
            match self.data.get(&id) {
                Some(inner) => &inner[start..(start + length)],
                None => panic!(),
            }
        } else {
            panic!()
        }
    }

    //pub fn get_char

    pub fn len(&self, id: SourceId) -> usize {
        if self.data.contains_key(&id) {
            match self.data.get(&id) {
                Some(inner) => inner.len(),
                None => panic!(),
            }
        } else {
            panic!()
        }
    }
}

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
