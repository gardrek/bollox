use std::sync::RwLock;
use string_interner::DefaultStringInterner;

pub type GenericResult<T = ()> = std::result::Result<T, Box<dyn std::error::Error>>;

pub mod arg;
pub mod interpreter;
pub mod parser;
pub mod source;

pub mod ast;
pub mod object;
pub mod result;
pub mod scanner;
pub mod token;

use interpreter::ErrorOrReturn;
use interpreter::Interpreter;
use parser::Parser;
use result::Result;
use scanner::Scanner;
use source::SourceId;

#[macro_use]
extern crate lazy_static;

extern crate string_interner;

lazy_static! {
    pub static ref INTERNER: RwLock<DefaultStringInterner> =
        RwLock::new(DefaultStringInterner::new());
    //~ pub static ref SOURCE: RwLock<String> = RwLock::new(String::new());
    /* pub static ref SOURCE: RwLock<HashMap<PathBuf, String>> =
        RwLock::new(HashMap::new()); */
}

pub fn run_string(
    source: String,
    id: usize,
    compatibility_mode: bool,
) -> Result<Option<object::Object>> {
    let mut parser = Parser::new(Scanner::new(&source, SourceId(id)));
    let statements = parser.parse_all()?;

    let had_error = !parser.errors.is_empty();

    if had_error {
        //~ if true {
        for e in parser.errors {
            eprintln!(
                "error on line {:?}: {}",
                crate::source::SourceLocation::error_line_number(
                    &result::Error::Parser(e.clone()),
                    &source
                ),
                e,
            )
        }

        //~ /*
        eprintln!();
        for s in &statements {
            //~ eprint!("{} ", s);
            eprintln!("{}", s);
        }
        eprintln!();
        //~ */
        return Ok(None);
    }

    let mut interpreter = Interpreter::new(compatibility_mode);

    interpreter.init_global_environment();
    interpreter.init_native_methods();

    let obj = match interpreter.interpret_slice(&statements[..]) {
        Ok(obj) => obj,
        Err(eor) => match eor {
            ErrorOrReturn::RuntimeError(e) => {
                let src = source::Source::new(&source);
                eprintln!("error on line {:?}", src.get_line_number(&e.location),);

                return Err(e.into());
            }
            ErrorOrReturn::Return(v) => v,
            ErrorOrReturn::Break(_v) => return Err(result::Error::BreakOutsideLoop),
        },
    };

    Ok(Some(obj))
}
