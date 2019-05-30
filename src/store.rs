#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct SourceId(usize);

use std::collections::{HashMap, HashSet};

/**
This store holds multiple source files, and can append to them, and give out slices of them.
**/
pub struct SourceStore {
    data: Vec<String>,
    //filename: HashMap<SourceId, String>,
    eof: HashSet<SourceId>,
}

impl SourceStore {
    pub fn new() -> Self {
        Self {
            data: vec![String::new()],
            eof: HashSet::new(),
        }
    }

    pub fn _add_empty(&mut self) -> SourceId {
        self.add_from_source(String::new())
    }

    /// Adds a new source file and returns an ID which can be used to access it.
    /// When a source file is removed, this ID becomes invalid.
    pub fn add_from_source(&mut self, source: String) -> SourceId {
        self.data.push(source);
        SourceId(self.data.len() - 1)
    }

    fn get(&self, id: SourceId) -> Option<&String> {
        Some(&self.data[id.0])
    }

    /*
    fn get_mut(&mut self, id: SourceId) -> Option<&mut String> {
        Some(&mut self.data[id.0])
    }

    // TODO: currently this and get_slice panic if an ID doesn't exist. could be improved
    pub fn push_str(&mut self, id: SourceId, s: &str) {
        let inner = self.get_mut(id).unwrap();
        inner.push_str(s)
    }
    */

    pub fn get_slice(&self, id: SourceId, start: usize, length: usize) -> &str {
        let inner = self.get(id).unwrap();
        &inner[start..(start + length)]
    }

    pub fn _get_eof(&mut self, id: &SourceId) -> bool {
        self.eof.contains(id)
    }

    pub fn set_eof(&mut self, id: SourceId) -> () {
        self.eof.insert(id);
    }

    //pub fn get_char

    pub fn len(&self, id: SourceId) -> usize {
        self.get(id).unwrap().len()
    }

    /*
    pub fn remove(&mut self, id: SourceId) {

    }
    */
}

use std::hash::Hash;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct StoreId(usize);

pub struct Store<T: Eq + Hash + Clone> {
    data: Vec<T>,
    map: HashMap<T, StoreId>,
    next_id: usize,
}

impl<T: Eq + Hash + Clone + Default> Store<T> {
    pub fn new() -> Self {
        let mut s = Self {
            data: vec![],
            map: HashMap::new(),
            next_id: 0,
        };
        let t = T::default();
        s.add(t);
        s
    }

    pub fn add(&mut self, item: T) -> StoreId {
        let index = self.next_id;
        let id = StoreId(index);
        self.map.insert(item.clone(), id);
        self.data.push(item);
        self.next_id += 1;
        id
    }

    pub fn get(&self, id: StoreId) -> Option<&T> {
        let index = id.0;
        if index < self.data.len() {
            Some(&self.data[index])
        } else {
            None
        }
    }

    pub fn _get_id(&self, item: &T) -> Option<&StoreId> {
        self.map.get(item)
    }
}
