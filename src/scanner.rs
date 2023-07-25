use crate::result;
use crate::source::{SourceId, SourceLocation};
use crate::token::{string_as_reserved_word, Operator, Token, TokenKind};
use crate::INTERNER;

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_digit(36) || c == '_'
}

//use std::path::PathBuf;

pub struct Scanner<'a> {
    // this is how we get the source file
    source: &'a str,
    // ID for the source
    source_id: SourceId,
    // location, in bytes which we're currently looking at
    cursor: usize,
    // whether eof was reached
    eof: bool,
    // whether scanning encountered an error
    had_error: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, source_id: SourceId) -> Scanner<'a> {
        Scanner {
            source,
            source_id,
            cursor: 0,
            eof: false,
            had_error: false,
        }
    }

    fn advance_char(&mut self) -> Option<char> {
        let index = self.cursor;
        if index < self.source.len() {
            let ch = self.source[index..].chars().next().unwrap();
            self.cursor += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    /*
    // This function is commented out because it could be useful in the future,
    // but it'd need to be modified to work with post-ASCII characters encoded in UTF-8
    fn lookahead<'a>(&self, source: &'a SourceReadGuard, length: usize) -> Option<&'a str> {
        let c = self.cursor;
        if (c + length) < source.len() {
            Some(&source[c..(c + length)])
        } else {
            None
        }
    }
    */

    fn peek_char(&self) -> Option<char> {
        self.source[self.cursor..].chars().next()
    }

    fn static_token(&mut self, kind: TokenKind, length: usize) -> Token {
        let location =
            SourceLocation::new(self.source_id.clone(), (self.cursor - length)..self.cursor);
        Token { location, kind }
    }

    fn match_static_token(
        &mut self,
        m: char,
        a: TokenKind,
        b: TokenKind,
        b_length: usize,
    ) -> Result<Token, result::Error> {
        if let Some(ch) = self.peek_char() {
            Ok(if ch == m {
                let a_length = ch.len_utf8();
                self.cursor += a_length;
                self.static_token(a, b_length + a_length)
            } else {
                self.static_token(b, b_length)
            })
        } else {
            Err(result::Error::Unimplemented(
                "match_static_token unexpected end of stream",
            ))
        }
    }

    fn match_static_operator(
        &mut self,
        m: char,
        a: Operator,
        b: Operator,
        b_length: usize,
    ) -> Result<Token, result::Error> {
        self.match_static_token(m, TokenKind::Op(a), TokenKind::Op(b), b_length)
    }

    fn line_comment(&mut self) {
        loop {
            let c = match self.advance_char() {
                None => return, // end of file
                Some(x) => x,
            };
            if c == '\n' {
                self.cursor -= 1;
                return;
            }
        }
    }

    fn block_comment(&mut self) -> Result<(), result::Error> {
        // TODO: This code seems to work but it's kind of a mess
        // Maybe come through later and spruce it up so it's less confusing to read
        // also we don't actually use the depth value at all
        match self.advance_char() {
            Some('/') => match self.peek_char() {
                Some('*') => {
                    self.cursor += 1;
                    loop {
                        match self.advance_char() {
                            None => {
                                return Err(result::Error::Unimplemented("Unclosed Block Comment"))
                            }
                            Some('*') => match self.peek_char() {
                                None => {
                                    return Err(result::Error::Unimplemented(
                                        "Unclosed Block Comment",
                                    ))
                                }
                                Some('/') => {
                                    self.cursor += 1;
                                    return Ok(());
                                }
                                Some(_) => continue,
                            },
                            Some('/') => match self.peek_char() {
                                None => {
                                    return Err(result::Error::Unimplemented(
                                        "Unclosed Block Comment",
                                    ))
                                }
                                Some('*') => {
                                    self.cursor -= 1;
                                    self.block_comment()?;
                                }
                                Some(_) => continue,
                            },
                            Some(_) => continue,
                        };
                    }
                }
                Some(_) => Err(result::Error::Unimplemented("not a Block Comment?")),
                None => Err(result::Error::Unimplemented("Unclosed Block Comment")),
            },
            Some(_) => Err(result::Error::Unimplemented("not a Block Comment?")),
            None => Err(result::Error::Unimplemented("Unclosed Block Comment")),
        }
    }

    fn number(&mut self) -> Token {
        let mut length = 0;
        let offset = self.cursor;

        let mut decimal = false;
        while let Some(ch) = self.peek_char() {
            if let Some(_digit) = ch.to_digit(10) {
                self.advance_char();
                length += ch.len_utf8();
            } else if ch == '.' {
                if decimal {
                    break;
                }
                self.cursor += ch.len_utf8();
                if let Some(ch_next) = self.peek_char() {
                    if ch_next.is_ascii_digit() {
                        length += ch_next.len_utf8();
                        decimal = true;
                    } else {
                        self.cursor -= ch.len_utf8();
                        break;
                    }
                } else {
                    self.cursor -= ch.len_utf8();
                    break;
                }
            } else {
                break;
            }
        }

        let location = SourceLocation::from_range(offset..(offset + length));

        let value = location.get_slice(self.source).parse::<f64>().ok().unwrap();

        Token {
            location,
            kind: TokenKind::Number(value),
        }
    }

    /* TODO: add escape sequence(s) at least for double quote */
    fn string(&mut self) -> Token {
        let offset = self.cursor - 1;
        let mut length = 0;

        loop {
            let c = self.peek_char();
            if let Some(c) = c {
                self.advance_char();
                length += c.len_utf8();
                if c == '"' {
                    length += 1;
                    break;
                }
            } else {
                let location = SourceLocation::from_range(offset..(offset + length));
                let token = Token {
                    location,
                    kind: TokenKind::UnfinishedString,
                };
                return token;
            }
        }

        let location = SourceLocation::from_range(offset..(offset + length));

        /*  Even though we're not multi-threaded yet,
            there's no reason to keep the lock longer than necessary
        */
        let sym = {
            let s = &location.get_slice(self.source)[1..length - 1];
            let mut interner = INTERNER.write().unwrap();
            interner.get_or_intern(s)
        };

        Token {
            location,
            kind: TokenKind::StaticString(sym),
        }
    }

    fn identifier(&mut self) -> Token {
        let offset = self.cursor;
        let mut length = 0;

        loop {
            let c = self.peek_char();
            if let Some(c) = c {
                if is_identifier_continue(c) {
                    self.advance_char();
                    length += c.len_utf8();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let location = SourceLocation::from_range(offset..(offset + length));

        let slicing = location.get_slice(self.source);

        let kind = if let Some(word) = string_as_reserved_word(slicing) {
            TokenKind::Reserved(word)
        } else {
            let sym = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern(slicing)
            };
            TokenKind::Identifier(sym)
        };

        Token { location, kind }
    }

    pub fn collect_all_tokens(&mut self) -> Result<Vec<Token>, (Vec<Token>, Vec<result::Error>)> {
        let mut tokens = vec![];
        let mut errors = None;

        loop {
            match self.next() {
                Some(result_token) => match result_token {
                    Ok(token) => tokens.push(token),
                    Err(error) => {
                        if errors.is_none() {
                            errors = Some(vec![]);
                        }
                        errors.as_mut().unwrap().push(error);
                    }
                },
                None => {
                    return match errors {
                        Some(e) => Err((tokens, e)),
                        None => Ok(tokens),
                    }
                }
            }
        }
    }

    fn error_unimplemented(&mut self, s: &'static str) -> result::Error {
        self.had_error = true;
        result::Error::Unimplemented(s)
    }

    fn do_eof(&mut self) -> Option<Token> {
        self.eof = true;
        None
        /*
        let offset = self.cursor;
        Some(Token {
            location: SourceLocation::from_range(offset..offset),
            kind: TokenKind::Eof,
        })
        // */
    }

    fn scan_token(&mut self) -> Result<Option<Token>, result::Error> {
        // TODO:  a big one; need to implement rewinding or some other solution to allow parsing
        // a token that is across multiple lines in interactive mode. need some way to signal EOF

        if self.eof {
            return Ok(None);
        }

        let total_length = self.source.len();

        // If we are past the end or already hit eof
        if self.cursor >= total_length {
            return Ok(self.do_eof());
        }

        let token = loop {
            // If there's no bytes left return None to signal the end
            let inner = match self.advance_char() {
                Some(ch) => ch,
                None => return Ok(self.do_eof()),
            };

            use Operator::*;
            use TokenKind::*;
            match inner {
                '(' => break self.static_token(LeftParen, 1),
                ')' => break self.static_token(RightParen, 1),
                '{' => break self.static_token(LeftBrace, 1),
                '}' => break self.static_token(RightBrace, 1),
                ',' => break self.static_token(Op(Comma), 1),
                '.' => break self.static_token(Op(Dot), 1),
                '-' => break self.match_static_operator('=', MinusEqual, Minus, 1)?,
                '+' => break self.match_static_operator('=', PlusEqual, Plus, 1)?,
                '/' => match self.peek_char() {
                    // no bytes left
                    None => return Ok(self.do_eof()),
                    Some(c) => match c {
                        '/' => {
                            self.line_comment();
                            continue;
                        }
                        '*' => {
                            self.cursor -= 1;
                            self.block_comment()?;
                            continue;
                        }
                        '=' => {
                            self.cursor += 1;
                            break self.static_token(Op(SlashEqual), 1)
                        }
                        _ => break self.static_token(Op(Slash), 1),
                    },
                },
                '*' => break self.match_static_operator('=', StarEqual, Star, 1)?,
                '%' => break self.match_static_operator('=', PercentEqual, Percent, 1)?,
                '!' => break self.match_static_operator('=', BangEqual, Bang, 1)?,
                '=' => break self.match_static_operator('=', EqualEqual, Equal, 1)?,
                '>' => break self.match_static_operator('=', GreaterEqual, Greater, 1)?,
                '<' => break self.match_static_operator('=', LessEqual, Less, 1)?,
                ';' => break self.static_token(Op(Semicolon), 1),
                '"' => break self.string(),
                ch if ch.is_whitespace() => {
                    continue;
                }
                ch if ch.is_ascii_digit() => {
                    self.cursor -= ch.len_utf8(); // TODO: maybe should do this another way?
                    break self.number();
                }
                ch if is_identifier_start(ch) => {
                    self.cursor -= ch.len_utf8();
                    break self.identifier();
                }
                // FIXME: This currently treats unrecognized chars as an error.
                // We now have full support for unicode inside strings, at least.
                // Perhaps we can add more Unicode awareness.
                // E.g. using Unicode designations to allow for post-ASCII identifiers.
                // It's unclear what the benefit of this would be, exactly, especially as no-one is going to use this language
                _ => return Err(self.error_unimplemented("Unrecognized Character Reached")),
            }
        };

        Ok(Some(token))
    }
}

impl Iterator for Scanner<'_> {
    type Item = Result<Token, result::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token().transpose()
    }
}

/*
#[derive(Debug, Clone)]
pub enum Error {
    UnclosedParenthesis,
    UnrecognizedCharacters(SourceLocation),
    ManyErrors(Vec<Error>),
    Unimplemented(&'static str),
}
*/
