use std::fmt;
use std::io;

//use crate::scanner::SourceLocation;

pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Usage,
    Io(io::Error),
    UnclosedParenthesis,
    ExpectedSemicolon,
    Unimplemented(&'static str),
    Ice(&'static str),
    Runtime(&'static str),
    //SyntaxError(SourceLocation<'static>),
    //Other(Box<dyn std::error::Error>),
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            //Unknown => write!(f, "Unkown Bollox Error"),
            Usage => write!(f, "Usage:\n    bollox <script>     Run a Lox script\n    bollox              Run in interactive mode"),
            Io(e) => write!(f, "{}", e),
            UnclosedParenthesis => write!(f, "Unclosed Parenthesis"),
            ExpectedSemicolon => write!(f, "Expected Semicolon"),
            Unimplemented(s) => write!(f, "Unimplemented feature: {}", s),
            Ice(s) => write!(f, "ICE: {}", s),
            Runtime(s) => write!(f, "Runtime Error: {}", s),
            //SyntaxError(location) => write!(f, "Syntax Error:\n{}", location),
            //Other(_) => write!(f, "{}", self),
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        // TODO: match on io::Error maybe?
        Error::Io(error)
    }
}

/*
impl<T: std::error::Error + 'static> From<T> for Error {
    fn from(error: T) -> Self {
        Error{
            kind: ErrorKind::Other(Box::new(error)),
            ..Default::default()
        }
    }
}
*/
