use crate::interpreter::RuntimeError;
use crate::parser::ParseError;
use crate::source::SourceLocation;
use std::fmt;
use std::io;

pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Error {
    Io,
    Unimplemented(&'static str),
    Runtime(RuntimeError),
    ManyErrors(Vec<Error>),
    Parser(ParseError),
    BreakOutsideLoop,
    Other(String),
}

impl Error {
    pub fn get_location(&self) -> Option<&SourceLocation> {
        Some(match self {
            Self::Parser(e) => &e.location,
            Self::Runtime(e) => &e.location,
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
            Error::Io => write!(f, "IO Error"),
            Unimplemented(s) => write!(f, "Unimplemented feature: {}", s),
            Runtime(s) => write!(f, "{}", s),
            ManyErrors(_) => write!(f, "Many Errors"),
            Parser(st) => write!(f, "{}", st),
            BreakOutsideLoop => write!(f, "Break Outside Loop"),
            Other(st) => write!(f, "{}", st),
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

impl From<String> for Error {
    fn from(error: String) -> Self {
        Error::Other(error)
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
