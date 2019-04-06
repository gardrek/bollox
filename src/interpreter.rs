use super::scanner::Scanner;
//use super::result::Result as LoxResult;
use super::result::Error as LoxError;

type LoxResult<T = ()> = Result<T, LoxError>;

/*
Derivables:
Eq, PartialEq,
Ord, PartialOrd
Clone, Copy,
Hash,
Default,
Debug,
*/

/*
use std::fs;
use std::path::Path;

use std::io;
use std::io::Write;
*/

pub struct Vm<'a> {
    source_store: Pin<Box<StringStore>>,
    //source: String,
    //raw_source: *const str,
    // Needs a scanner so it can scan new lines of source in interactive mode
    scanner: Scanner<'a>,
}

impl<'a> Vm<'a> {
    fn from_source(source: String) -> Self {
        let source_store = StringStore::new(source);
        Self {
            source_store,
            scanner: Scanner::new(""),
        }
    }

    //fn init_from_file(path: &Path) -> LoxResult<Self> {
    //let source = fs::read_to_string(path)?;
    //Ok((source, Vm::from_source(source)))
    //}

    fn run(&mut self) -> LoxResult {
        loop {
            let thing = self.scanner.scan_token();
            match thing {
                Some(token) => eprintln!("{:?}", token),
                None => break,
            }
        }
        //let sc = self.scanner;
        //for t in sc {
        //eprintln!("{:?}", t);
        //}
        //Err(Box::new(result::Error::Other(Box::new(io::Error::new(io::ErrorKind::Other, "ohnoes"))))
        Ok(())
    }

    //fn run_prompt(&mut self) -> LoxResult {
    //// TODO: make this work and loop and stuff.
    //// probably do need to keep persistent state in a struct
    //// TODO: use io::BufReader because it is better for this use case
    //let mut out = io::LineWriter::new(io::stdout());
    //loop {
    //write!(out, "λ ")?;
    //out.flush()?;
    //let mut line = String::new();
    //io::stdin().read_line(&mut line)?;
    //match self.run() {
    //Ok(_) => (),
    //Err(_) => break,
    //}
    //}
    //Ok(())
    //}
}

use std::marker::PhantomPinned;
use std::pin::Pin;
use std::ptr::NonNull;

// This is a self-referential struct since the slice field points to the data field.
// We cannot inform the compiler about that with a normal reference,
// since this pattern cannot be described with the usual borrowing rules.
// Instead we use a raw pointer, though one which is known to not be null,
// since we know it's pointing at the string.
struct StringStore {
    data: String,
    slice: NonNull<String>,
    _pin: PhantomPinned,
}

impl StringStore {
    // To ensure the data doesn't move when the function returns,
    // we place it in the heap where it will stay for the lifetime of the object,
    // and the only way to access it would be through a pointer to it.
    fn new(data: String) -> Pin<Box<Self>> {
        let res = Self {
            data,
            // we only create the pointer once the data is in place
            // otherwise it will have already moved before we even started
            slice: NonNull::dangling(),
            _pin: PhantomPinned,
        };
        let mut boxed = Box::pin(res);

        let slice = NonNull::from(&boxed.data);
        // we know this is safe because modifying a field doesn't move the whole struct
        unsafe {
            let mut_ref: Pin<&mut Self> = Pin::as_mut(&mut boxed);
            Pin::get_unchecked_mut(mut_ref).slice = slice;
        }
        boxed
    }
}
