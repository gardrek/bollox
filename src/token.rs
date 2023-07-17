use crate::source::SourceLocation;
use string_interner::Sym;

use std::fmt;

#[derive(Debug, Clone)]
pub struct Token {
    pub location: SourceLocation,
    pub kind: TokenKind,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    // Single-character symbols.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Operators, of course
    Op(Operator),

    // Literals
    Number(f64),
    StaticString(Sym),
    UnfinishedString,
    Identifier(Sym),

    Reserved(ReservedWord),
    // The book has an End of File token, which may come in handy later
    //~ Eof,
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
    Break,
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
}

impl Token {
    pub fn in_kinds(&self, kinds: &[TokenKind]) -> bool {
        for k in kinds {
            if self.kind.same_kind(k) {
                return true;
            }
        }
        false
    }

    pub fn is_object(&self) -> bool {
        match &self.kind {
            TokenKind::Number(_value) => true,
            TokenKind::StaticString(_sym) => true,
            TokenKind::Reserved(word) => matches!(
                word,
                ReservedWord::True | ReservedWord::False | ReservedWord::Nil
            ),
            _ => false,
        }
    }

    pub fn to_operator(&self) -> Operator {
        match &self.kind {
            TokenKind::Op(op) => op.clone(),
            _ => panic!("called to_operator on non-operator token"),
        }
    }
}

impl TokenKind {
    pub fn same_kind(&self, other: &Self) -> bool {
        use TokenKind::*;
        match (self, other) {
            (LeftParen, LeftParen)
            | (RightParen, RightParen)
            | (LeftBrace, LeftBrace)
            | (RightBrace, RightBrace)
            | (Number(_), Number(_))
            | (StaticString(_), StaticString(_))
            | (UnfinishedString, UnfinishedString)
            | (Identifier(_), Identifier(_)) => true, //~ | (Eof, Eof) => true,

            (Reserved(word_a), Reserved(word_b)) => word_a == word_b,

            (Op(op_a), Op(op_b)) => op_a == op_b,

            /*  The reason we don't replace this with just `_ => false` is so that if a new
            token type is added, you have to come here and properly add it's true condition.
            we also allow unreachable patterns here because technically, whatever token you
            list last will technically be an unreachable pattern, for the same reason. */
            #[allow(unreachable_patterns)]
            (LeftParen, _)
            | (_, LeftParen)
            | (RightParen, _)
            | (_, RightParen)
            | (LeftBrace, _)
            | (_, LeftBrace)
            | (RightBrace, _)
            | (_, RightBrace)
            | (Number(_), _)
            | (_, Number(_))
            | (StaticString(_), _)
            | (_, StaticString(_))
            | (UnfinishedString, _)
            | (_, UnfinishedString)
            | (Identifier(_), _)
            | (_, Identifier(_))
            | (Reserved(_), _)
            | (_, Reserved(_))
            | (Op(_), _)
            | (_, Op(_)) => false,
            //~ | (Eof, _)
            //~ | (_, Eof) => false,
        }
    }
}

pub fn reserved_word_as_string(s: &ReservedWord) -> &'static str {
    use ReservedWord::*;
    match s {
        And => "and",
        Break => "break",
        Class => "class",
        Else => "else",
        False => "false",
        Fun => "fun",
        For => "for",
        If => "if",
        Nil => "nil",
        Or => "or",
        Print => "print",
        Return => "return",
        Super => "super",
        This => "this",
        True => "true",
        Var => "var",
        While => "while",
    }
}

pub fn string_as_reserved_word(s: &str) -> Option<ReservedWord> {
    use ReservedWord::*;
    Some(match s {
        "and" => And,
        "break" => Break,
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
        _ => return None,
    })
}

pub fn operator_as_string(s: &Operator) -> &'static str {
    use Operator::*;
    match s {
        Comma => ",",
        Dot => ".",
        Minus => "-",
        Plus => "+",
        Semicolon => ";",
        Slash => "/",
        Star => "*",
        Bang => "!",
        BangEqual => "!=",
        Equal => "=",
        EqualEqual => "==",
        Greater => ">",
        GreaterEqual => ">=",
        Less => "<",
        LessEqual => "<=",
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenKind::*;

        match &self {
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),

            Op(op) => write!(f, "{}", op),

            Number(num) => write!(f, "{}", num),
            StaticString(sym) => write!(f, "StaticString#{:?}", sym),
            UnfinishedString => write!(f, "UnfinishedString"),
            Identifier(sym) => write!(f, "ID#{:?}", sym),
            Reserved(word) => write!(f, "#{}", reserved_word_as_string(word)),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", operator_as_string(self))
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
// */
