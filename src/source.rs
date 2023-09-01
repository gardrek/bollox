use std::fmt;

pub struct Source {
    pub src: String,
}

impl Source {
    pub fn new(s: &str) -> Source {
        Source { src: s.to_string() }
    }

    pub fn push(&mut self, s: &str) {
        self.src.push_str(s);
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.src.len()
    }

    pub fn get_slice(&self, loc: &SourceLocation) -> &str {
        &self.src[loc.range.clone()]
    }

    pub fn peek_char(&self, cursor: usize) -> Option<char> {
        self.src[cursor..].chars().next()
    }

    pub fn get_line_number(&self, loc: &SourceLocation) -> usize {
        'l: {
            let mut line_number = 1;

            for (index, byte) in self.src.as_bytes().iter().enumerate() {
                if loc.range.start == index {
                    break 'l line_number;
                }

                if *byte == b'\n' {
                    line_number += 1;
                }
            }

            panic!("ICE: Source Location outside source at {}", loc);
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceId(pub usize);

#[derive(Debug, Clone)]
pub struct SourceLocation {
    // TODO: need some way to get which file a SourceLocation refers to
    // NOTE: don't insert a source_span here, there's no reason to pre-calculate line numbers
    // when we won't need them until there's an error

    // Use this to get the source file or string that this source location is from
    source: SourceId,

    // byte-indexed location from the start of the file/string
    pub range: std::ops::Range<usize>,
}

impl SourceLocation {
    pub fn error_line_number(e: &crate::result::Error, source: &str) -> Option<usize> {
        let loc = if let Some(loc) = e.get_location() {
            loc.clone()
        } else {
            return None;
        };

        Some('l: {
            let mut line_number = 1;

            for (index, byte) in source.as_bytes().iter().enumerate() {
                if loc.range.start == index {
                    break 'l line_number;
                }

                if *byte == b'\n' {
                    line_number += 1;
                }
            }

            return None;
        })
    }

    pub fn get_slice<'a>(&self, source: &'a str) -> &'a str {
        &source[self.range.clone()]
    }

    pub fn from_range(range: std::ops::Range<usize>) -> SourceLocation {
        SourceLocation {
            range,
            source: SourceId(0),
        }
    }

    pub fn bullshit() -> SourceLocation {
        SourceLocation {
            range: 0..0,
            source: SourceId(0),
        }
    }

    pub fn new(source: SourceId, range: std::ops::Range<usize>) -> SourceLocation {
        SourceLocation { source, range }
    }

    pub fn _len(&self) -> usize {
        self.range.len()
    }

    pub fn _id(&self) -> &SourceId {
        &self.source
    }

    pub fn combine(&self, other: &SourceLocation) -> SourceLocation {
        if self.source != other.source {
            panic!()
        }

        // let's just make sure that our ranges aren't upside-down
        // that is, that they don't end before they start
        fn range_is_upside_down(range: &std::ops::Range<usize>) -> bool {
            range.start > range.end
        }

        if range_is_upside_down(&self.range) {
            panic!()
        }

        if range_is_upside_down(&other.range) {
            panic!()
        }

        // now that we know they're not upside-down, the first start is the smallest index...
        let start = { self.range.start.min(other.range.start) };
        // ... and the last end is the largest index
        let end = { self.range.end.max(other.range.end) };
        // so this constructs the smallest range that contains both input ranges

        SourceLocation {
            source: self.source.clone(),
            range: start..end,
        }
    }

    /*
    fn file_path<'a>(&self) -> &'a str {
        unimplemented!()
    }

    fn line_number(&self) -> usize {
        unimplemented!()
    }

    fn column(&self) -> usize {
        unimplemented!()
    }
    */
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}..{}",
            self.range.start,
            self.range.end,
            /*
            "  --> {} {}:{}"
            self.file_path(),
            self.line_number(),
            self.column(),
            */
        )
    }
}
