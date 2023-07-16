use crate::interpreter::RuntimeError;
use crate::source::SourceLocation;
use std::fmt;
use std::io;

//use crate::scanner::SourceLocation;

pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Error {
    Usage,
    //~ Io(io::Error),
    Io,
    Unimplemented(&'static str),
    //~ Ice(&'static str),
    Runtime(RuntimeError),
    ManyErrors(Vec<Error>),
    Parser(crate::parser::ParseError),
    //SyntaxError(SourceLocation<'static>),
    //Other(Box<dyn std::error::Error>),
}

impl Error {
    fn _report(&self, location: &SourceLocation) {
        eprintln!("{} at {}", self, location)
    }

    pub fn get_location(&self) -> Option<&SourceLocation> {
        Some(match self {
            Self::Parser(e) => &e.location,
            e => {
                eprintln!("error location unimplemented for {}", e);
                return None;
            }
        })
    }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            //~ Unknown => write!(f, "Unkown Bollox Error"),
            //~ Usage => write!(f, "Usage:\n    bollox <script>     Run a Lox script\n    bollox              Run in interactive mode"),
            //~ Io(e) => write!(f, "{}", e),
            Usage => write!(
                f,
                "\
Usage:
    bollox <script>         Run a Lox script
    bollox                  Run in interactive mode"
            ),
            Io => write!(f, "IO Error"),
            //~ UnclosedParenthesis => write!(f, "Unclosed Parenthesis"),
            //~ ExpectedSemicolon => write!(f, "Expected Semicolon"),
            //~ ExpectedIdentifier => write!(f, "Expected Identifier"),
            //~ UnexpectedToken(t) => write!(f, "Expected Token {:?}", t),
            Unimplemented(s) => write!(f, "Unimplemented feature: {}", s),
            //~ Ice(s) => write!(f, "ICE: {}", s),
            Runtime(s) => write!(f, "{}", s),
            ManyErrors(_) => write!(f, "Many Errors"),
            Parser(st) => write!(f, "{}", st),
            //SyntaxError(location) => write!(f, "Syntax Error:\n{}", location),
            //Other(_) => write!(f, "{}", self),
        }
    }
}

impl From<io::Error> for Error {
    fn from(_error: io::Error) -> Self {
        // TODO: match on io::Error maybe?
        //~ Error::Io(error)
        Error::Io
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
