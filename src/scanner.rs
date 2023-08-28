use crate::result;
use crate::source::{Source, SourceId, SourceLocation};
use crate::token::{string_as_reserved_word, Operator, Token, TokenKind};
use crate::INTERNER;

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_digit(36) || c == '_'
}

pub struct Scanner {
    // this is how we get the source file
    source: Source,
    // ID for the source
    source_id: SourceId,
    // location, in bytes which we're currently looking at
    cursor: usize,
    // whether eof was reached
    eof: bool,
    // whether scanning encountered an error
    had_error: bool,
    // whether or not compatibility mode is enabled
    compatibility: bool,
}

impl Scanner {
    pub fn new(src: &str, source_id: SourceId, compatibility: bool) -> Scanner {
        Scanner {
            source: Source::new(src),
            source_id,
            cursor: 0,
            eof: false,
            had_error: false,
            compatibility,
        }
    }

    pub fn push_source_string(&mut self, s: &str) {
        self.eof = false;
        self.source.push(s)
    }

    fn advance_char(&mut self) -> Option<char> {
        let index = self.cursor;
        if index < self.source.len() {
            let ch = self.peek_char().unwrap();
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
        self.source.peek_char(self.cursor)
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
        let offset = self.cursor;

        if let Some('0') = self.peek_char() {
            self.advance_char();
            if let Some(ch) = self.peek_char() {
                match ch {
                    'x' => {
                        let len = 1 + ch.len_utf8();
                        self.advance_char();
                        self.hex_int(offset, len)
                    }
                    _ => self.float(offset, 1),
                }
            } else {
                self.float(offset, 0)
            }
        } else {
            self.float(offset, 0)
        }
    }

    fn hex_int(&mut self, offset: usize, length_so_far: usize) -> Token {
        let mut length = length_so_far;

        while let Some(ch) = self.peek_char() {
            if let Some(_digit) = ch.to_digit(16) {
                self.advance_char();
                length += ch.len_utf8();
            } else {
                break;
            }
        }

        let location = SourceLocation::from_range(offset..(offset + length));

        let slice = self.source.get_slice(&location).strip_prefix("0x").unwrap();

        let value = match i64::from_str_radix(slice, 16) {
            Ok(v) => v as f64,
            Err(_) => {
                return Token {
                    location,
                    kind: TokenKind::InvalidNumber,
                }
            }
        };

        Token {
            location,
            kind: TokenKind::Number(value),
        }
    }

    fn float(&mut self, offset: usize, length_so_far: usize) -> Token {
        let mut length = length_so_far;

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

        let slice = self.source.get_slice(&location);

        let value = slice.parse::<f64>().unwrap();

        Token {
            location,
            kind: TokenKind::Number(value),
        }
    }

    fn string(&mut self) -> Token {
        let offset = self.cursor - 1;
        let mut length = 0;

        loop {
            let c = self.peek_char();
            if let Some(c) = c {
                self.advance_char();
                length += c.len_utf8();
                if c == '\\' {
                    if let Some(c) = self.peek_char() {
                        self.advance_char(); // jump forward one to skip the quote check
                        length += c.len_utf8();
                    } else {
                        let location = SourceLocation::from_range(offset..(offset + length));
                        let token = Token {
                            location,
                            kind: TokenKind::UnfinishedString,
                        };
                        return token;
                    }
                } else if c == '"' {
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
        let raw = &self.source.get_slice(&location)[1..length - 1];

        // TODO: actually handle errors here, possibly introduce a new errors holder, or throw this down the line to the parser
        let s = match Scanner::parse_string(raw) {
            Ok(s) => s,
            Err(_e) => {
                self.had_error = true;
                "".to_string()
            }
        };

        let sym = {
            let mut interner = INTERNER.write().unwrap();
            interner.get_or_intern(s)
        };

        Token {
            location,
            kind: TokenKind::StaticString(sym),
        }
    }

    fn parse_string(escaped_string: &str) -> Result<String, &'static str> {
        let mut final_string = String::with_capacity(escaped_string.len());
        let mut iter = escaped_string.chars();

        while let Some(ch) = iter.next() {
            final_string.push(match ch {
                '\\' => match iter.next() {
                    Some(esc) => match esc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '0' => '\0',
                        '\'' => '\'',
                        '\"' => '\"',
                        'x' => return Err("unimplemented string escape `\\x`"),
                        'u' => return Err("unimplemented string escape `\\u`"),
                        _ => return Err("unrecognized string escape"),
                    },
                    None => return Err("unexpected end of string"),
                },
                _ => ch,
            });
        }

        Ok(final_string)
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

        let slicing = self.source.get_slice(&location);

        let kind = if let Some(word) = string_as_reserved_word(slicing) {
            if word.is_modal() && self.compatibility {
                let sym = {
                    let mut interner = INTERNER.write().unwrap();
                    interner.get_or_intern(slicing)
                };
                TokenKind::Identifier(sym)
            } else {
                TokenKind::Reserved(word)
            }
        } else {
            let sym = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern(slicing)
            };
            TokenKind::Identifier(sym)
        };

        Token { location, kind }
    }

    fn error_unimplemented(&mut self, s: &'static str) -> result::Error {
        self.had_error = true;
        result::Error::Unimplemented(s)
    }

    fn do_eof(&mut self) -> Option<Token> {
        self.eof = true;
        None
    }

    fn scan_token(&mut self) -> Result<Option<Token>, result::Error> {
        let total_length = self.source.len();

        // If we are past the end or already hit eof
        if self.eof || self.cursor >= total_length {
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
                '[' => break self.static_token(LeftBracket, 1),
                ']' => break self.static_token(RightBracket, 1),
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
                            break self.static_token(Op(SlashEqual), 1);
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
                    self.cursor -= ch.len_utf8();
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

impl Iterator for Scanner {
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
