use crate::scanner::Scanner;
use crate::store::SourceStore;



pub fn test_run(source: String) {
    let mut store = SourceStore::new();
    let id = store.add_empty();
    store.push_str(id, &source);

    let mut sc = Scanner::new(id);
    loop {
        match sc.scan_token(&store) {
            Some(token) => eprintln!("{}: {}", token, token.get_slice(&store)),
            None => break,
        }
    }

    store.remove(id);
}

