use crate::parser::Parser;
use crate::scanner::{Scanner, TokenKind};
use crate::store::SourceStore;
use crate::store::Store;
use crate::result::Result;

pub fn run_string(source: String) -> Result<String> {
    let mut store = SourceStore::new();

    // move the source in, no need to allocate again
    let id = store.add_from_source(source);

    store.set_eof(id);

    let mut id_store = Store::new(String::from(""));
    let mut string_store = Store::new(String::from(""));

    let mut sc = Scanner::new(id);
    let mut tokens = sc.collect_or_first_error(&store)?;

    for token in tokens.iter_mut() {
        match token.kind() {
            TokenKind::Identifier(_) => token.intern_identifier(&store, &mut id_store),
            TokenKind::StaticString(_) => token.intern_string(&store, &mut string_store),
            _ => None,
        };
    }

    let mut parser = Parser::new(tokens);
    let expr = parser.parse();

    // TODO: eval and return result here
    Ok(format!("{:?}", expr))
}
