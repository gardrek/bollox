#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct SourceId(usize);

use std::collections::HashMap;

/**
This store holds multiple source files, and can append to them, and give out slices of them.
**/
pub struct SourceStore {
    data: HashMap<SourceId, String>,
    next_id: usize,
}

impl SourceStore {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            next_id: 1,
        }
    }

    pub fn add_empty(&mut self) -> SourceId {
        self.add_from_source(String::new())
    }

    /// Adds a new source file and returns an ID which can be used to access it.
    /// When a source file is removed, this ID becomes invalid.
    pub fn add_from_source(&mut self, source: String) -> SourceId {
        let id = SourceId(self.next_id);
        if self.data.contains_key(&id) {
            panic!("SourceStore tried to create ID that already exists.");
        }
        self.data.insert(id, source);
        self.next_id += 1;
        id
    }

    fn get(&self, id: SourceId) -> Option<&String> {
        self.data.get(&id)
    }

    fn get_mut(&mut self, id: SourceId) -> Option<&mut String> {
        self.data.get_mut(&id)
    }

    // TODO: currently this and get_slice panic if an ID doesn't exist. could be improved
    pub fn push_str(&mut self, id: SourceId, s: &str) {
        let inner = self.get_mut(id).unwrap();
        inner.push_str(s)
    }

    pub fn get_slice(&self, id: SourceId, start: usize, length: usize) -> &str {
        let inner = self.get(id).unwrap();
        &inner[start..(start + length)]
    }

    //pub fn get_char

    pub fn len(&self, id: SourceId) -> usize {
        self.get(id).unwrap().len()
    }

    pub fn remove(&mut self, id: SourceId) {
        if self.data.contains_key(&id) {
            self.data.remove(&id);
        } else {
            panic!("tried to remove nonexistant source from SourceStore");
        }
    }
}

//*
#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Identifier(usize);

pub struct IdentifierStore {
    data: Vec<String>,
    next_id: usize,
}

impl IdentifierStore {
    pub fn new() -> Self {
        Self {
            data: vec![String::from("")],
            next_id: 1,
        }
    }

    pub fn add(&mut self, s: String) -> Identifier {
        let index = self.next_id;
        self.next_id += 1;
        self.data.push(s);
        Identifier(index)
    }

    fn get(&self, id: Identifier) -> Option<&String> {
        let index = id.0;
        if index < self.data.len() {
            Some(&self.data[index])
        } else {
            None
        }
    }
}
// */
