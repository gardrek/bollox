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
    LeftBracket,
    RightBracket,

    // Operators, of course
    Op(Operator),

    // Literals
    Number(f64),
    InvalidNumber,
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
    Slash,
    Star,
    Percent,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Semicolon,
    MinusEqual,
    PlusEqual,
    SlashEqual,
    StarEqual,
    PercentEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReservedWord {
    And,
    Break,
    Class,
    Const,
    Else,
    False,
    Fun,
    For,
    Global,
    If,
    In,
    Local,
    Mut,
    Nil,
    Or,
    Print,
    Return,
    Super,
    Switch,
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
            | (LeftBracket, LeftBracket)
            | (RightBracket, RightBracket)
            | (Number(_), Number(_))
            | (InvalidNumber, InvalidNumber)
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
            | (LeftBracket, _)
            | (_, LeftBracket)
            | (RightBracket, _)
            | (_, RightBracket)
            | (Number(_), _)
            | (_, Number(_))
            | (InvalidNumber, _)
            | (_, InvalidNumber)
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

impl ReservedWord {
    pub fn is_modal(&self) -> bool {
        use ReservedWord as Rw;
        !matches!(
            self,
            Rw::And
                | Rw::Class
                | Rw::Else
                | Rw::False
                | Rw::Fun
                | Rw::For
                | Rw::If
                | Rw::Nil
                | Rw::Or
                | Rw::Print
                | Rw::Return
                | Rw::Super
                | Rw::This
                | Rw::True
                | Rw::Var
                | Rw::While
        )
    }
}
pub fn reserved_word_as_string(s: &ReservedWord) -> &'static str {
    use ReservedWord as Rw;
    match s {
        Rw::And => "and",
        Rw::Break => "break",
        Rw::Class => "class",
        Rw::Const => "const",
        Rw::Else => "else",
        Rw::False => "false",
        Rw::Fun => "fun",
        Rw::For => "for",
        Rw::Global => "global",
        Rw::If => "if",
        Rw::In => "in",
        Rw::Local => "local",
        Rw::Mut => "mut",
        Rw::Nil => "nil",
        Rw::Or => "or",
        Rw::Print => "print",
        Rw::Return => "return",
        Rw::Super => "super",
        Rw::Switch => "switch",
        Rw::This => "this",
        Rw::True => "true",
        Rw::Var => "var",
        Rw::While => "while",
    }
}

pub fn string_as_reserved_word(s: &str) -> Option<ReservedWord> {
    use ReservedWord as Rw;
    Some(match s {
        "and" => Rw::And,
        "break" => Rw::Break,
        "class" => Rw::Class,
        "const" => Rw::Const,
        "else" => Rw::Else,
        "false" => Rw::False,
        "fun" => Rw::Fun,
        "for" => Rw::For,
        "global" => Rw::Global,
        "if" => Rw::If,
        "in" => Rw::In,
        "local" => Rw::Local,
        "mut" => Rw::Mut,
        "nil" => Rw::Nil,
        "or" => Rw::Or,
        "print" => Rw::Print,
        "return" => Rw::Return,
        "super" => Rw::Super,
        "switch" => Rw::Switch,
        "this" => Rw::This,
        "true" => Rw::True,
        "var" => Rw::Var,
        "while" => Rw::While,
        _ => return None,
    })
}

pub fn operator_as_string(s: &Operator) -> &'static str {
    use Operator as Op;
    match s {
        Op::Comma => ",",
        Op::Dot => ".",
        Op::Minus => "-",
        Op::Plus => "+",
        Op::Slash => "/",
        Op::Star => "*",
        Op::Percent => "%",
        Op::Bang => "!",
        Op::BangEqual => "!=",
        Op::Equal => "=",
        Op::EqualEqual => "==",
        Op::Greater => ">",
        Op::GreaterEqual => ">=",
        Op::Less => "<",
        Op::LessEqual => "<=",
        Op::Semicolon => ";",
        Op::MinusEqual => "-=",
        Op::PlusEqual => "+=",
        Op::SlashEqual => "/=",
        Op::StarEqual => "*=",
        Op::PercentEqual => "%=",
    }
}

impl Operator {
    pub fn binary_from_combined(&self) -> Option<Operator> {
        use Operator as Op;
        Some(match self {
            Op::MinusEqual => Op::Minus,
            Op::PlusEqual => Op::Plus,
            Op::SlashEqual => Op::Slash,
            Op::StarEqual => Op::Star,
            Op::PercentEqual => Op::Percent,
            _ => return None,
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenKind as Tk;

        match &self {
            Tk::LeftParen => write!(f, "("),
            Tk::RightParen => write!(f, ")"),
            Tk::LeftBrace => write!(f, "{{"),
            Tk::RightBrace => write!(f, "}}"),
            Tk::LeftBracket => write!(f, "["),
            Tk::RightBracket => write!(f, "]"),

            Tk::Op(op) => write!(f, "{}", op),

            Tk::Number(num) => write!(f, "{}", num),
            Tk::InvalidNumber => write!(f, "InvalidNumber"),
            Tk::StaticString(sym) => write!(f, "StaticString#{:?}", sym),
            Tk::UnfinishedString => write!(f, "UnfinishedString"),
            Tk::Identifier(sym) => write!(f, "{}", crate::object::sym_to_str(sym)),
            Tk::Reserved(word) => write!(f, "#{}", reserved_word_as_string(word)),
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
