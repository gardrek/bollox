use crate::scanner::Scanner;
use crate::store::{SourceStore, IdentifierStore};

pub fn test_run(source: String) {
    let mut store = SourceStore::new();
    let id = store.add_empty();
    store.push_str(id, &source);

    let mut id_store = IdentifierStore::new();

    let mut sc = Scanner::new(id);
    loop {
        match sc.scan_token(&store) {
            Some(mut token) => {
                eprintln!("\n{}: {}", token, token.get_slice(&store));
                token.intern_identifier(&store, &mut id_store);
            },
            None => break,
        }
    }

    store.remove(id);
}
