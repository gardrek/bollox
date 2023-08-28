use std::sync::RwLock;
use string_interner::DefaultStringInterner;

pub type GenericResult<T = ()> = std::result::Result<T, Box<dyn std::error::Error>>;

pub mod arg;
pub mod interpreter;
pub mod parser;
pub mod source;

pub mod ast;
pub mod object;
pub mod resolver;
pub mod result;
pub mod scanner;

mod token;

mod stdlib;

use interpreter::ControlFlow;
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
}

fn parse_and_resolve(
    source: &str,
    id: usize,
    compatibility_mode: bool,
) -> Result<Vec<ast::Stmt>> {
    let mut parser = Parser::new(
        Scanner::new(source, SourceId(id), compatibility_mode),
        compatibility_mode,
    );

    let mut statements = parser.parse_all()?;

    let had_error = !parser.errors().is_empty();

    if had_error {
        for e in parser.errors().iter() {
            eprintln!(
                "error on line {:?}: {} `{}`",
                crate::source::SourceLocation::error_line_number(
                    &result::Error::Parser(e.clone()),
                    source
                ),
                e,
                e.get_slice(source),
            )
        }

        /*
        eprintln!();
        for s in &statements {
            eprintln!("{}", s);
        }
        eprintln!();
        //~ */

        return Err(result::Error::ManyErrors(
            parser.errors().iter().map(|e| e.clone().into()).collect(),
        ));
    }

    let mut resolver = resolver::Resolver::default();

    resolver.resolve_all(&mut statements[..]);

    let mut had_error = false;

    for err in resolver.errors.iter() {
        eprintln!("resolver error: {}", err);
        had_error = true;
    }

    for warn in resolver.warnings.iter() {
        eprintln!("resolver warning: {}", warn);
    }

    if had_error {
        return Err(result::Error::ManyErrors(
            resolver.errors.into_iter().map(|e| e.into()).collect(),
        ));
    }

    Ok(statements)
}

pub fn run_string(
    source: String,
    id: usize,
    compatibility_mode: bool,
) -> Result<Option<object::Object>> {
    let mut interpreter = Interpreter::new_with_stdlib();

    interpret(&source, id, compatibility_mode, &mut interpreter)
}


pub fn interpret(
    source: &str,
    id: usize,
    compatibility_mode: bool,
    interpreter: &mut Interpreter,
) -> Result<Option<object::Object>> {
    let statements = parse_and_resolve(&source, id, compatibility_mode)?;

    let obj = match interpreter.interpret_slice(&statements) {
        Ok(obj) => obj,
        Err(eor) => match eor {
            ControlFlow::RuntimeError(e) => {
                let src = source::Source::new(source);
                eprintln!("error on line {:?}", src.get_line_number(&e.location),);

                return Err(e.into());
            }
            ControlFlow::Return(v) => v,
            ControlFlow::Break(_v) => return Err(result::Error::BreakOutsideLoop),
        },
    };

    Ok(Some(obj))
}
