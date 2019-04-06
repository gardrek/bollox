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
            self.kind,
            self.location.offset,
            self.location.length,
        )
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    //LeftParen, RightParen, LeftBrace, RightBrace,
    //Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    //Literal(Literal),
    Eof,
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

    /*fn lookahead<'a>(&mut self, source: &'a str, len: usize) -> &'a str {
        let c = self.cursor;
        &source[c..(c + len)]
    }*/

    pub fn scan_token(&mut self, store: &SourceStore) -> Option<Token> {
        let current = self.cursor;
        let offset = current;
        let length;
        let total_length = store.len(self.source_id);

        if current <= total_length {
            let test_length = 1;
            let kind;

            //length = store.len(self.source_id);

            self.cursor += test_length;

            let slice = store.get_slice(self.source_id, current, total_length.saturating_sub(current));
            if slice.len() != 0 {
                length = 1;
                kind = TokenKind::Eof;
            } else {
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
        } else {
            None
        }
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
