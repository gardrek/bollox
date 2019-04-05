use std::fmt;
//use super::scanner::SourceLocation;

//pub type Result<T = ()> = std::result::Result<T, Box<Error>>;

#[derive(Debug)]
pub enum Error {
    None,
    Usage,
    //SyntaxError(SourceLocation<'static>),
    //Other(Box<dyn std::error::Error>),
}

impl std::error::Error for Error {}

impl std::default::Default for Error {
    fn default() -> Self {
        Error::None
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            None => write!(f, "Unkown Bollox Error"),
            Usage => write!(f, "Usage:\n    bollox <script>     Run a script\n    bollox              Run in interactive mode"),
            //SyntaxError(location) => write!(f, "Syntax Error:\n{}", location),
            //Other(_) => write!(f, "{}", self),
        }
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
