use std::fmt;

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
    range: std::ops::Range<usize>,
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

/*
pub struct SourceLines {
    //~ source: SourceId,

    // byte-indexed location from the start of the file/string
    ranges: Vec<std::ops::Range<usize>>,
}

impl SourceLines {
    pub fn new(string: &str) -> SourceLines {
        let ranges = std::iter::once(0)
            .chain(string.char_indices().filter_map(|(byte_index, ch)| {
                if ch == '\n' {
                    Some(byte_index + 1)
                } else {
                    None
                }
            }))
            .chain(std::iter::once(string.len()))
            .collect::<Vec<usize>>()
            .windows(2)
            .map(|slice| match slice {
                &[x, y] => x..y,
                _ => unreachable!(),
            })
            .collect();

        SourceLines { ranges }
    }

    pub fn line_number_of_byte_index(&self, byte_index: usize) -> Option<usize> {
        self.ranges
            .iter()
            .enumerate()
            .position(|(_line_index, range)| range.contains(&byte_index))
    }

    pub fn range(&self, line_number: usize) -> std::ops::Range<usize> {
        self.ranges[line_number].clone()
    }
}
#[cfg(test)]
mod tests {
    #[test]
    fn lines_test() {
        let src_string = "check me out\nI'm some lox code\r\nfor real\n";
        let lines = super::SourceLines::new(&src_string);

        assert_eq!(lines.ranges.len(), 4);

        assert_eq!("check me out\n", &src_string[lines.range(0)]);
        assert_eq!("I'm some lox code\r\n", &src_string[lines.range(1)]);
        assert_eq!("for real\n", &src_string[lines.range(2)]);
        assert_eq!("", &src_string[lines.range(3)]);
    }

    #[test]
    fn lines_test_no_final_line_feed() {
        let src_string = "check me out\nI'm some lox code\r\nfor real";
        let lines = super::SourceLines::new(&src_string);

        assert_eq!(lines.ranges.len(), 3);

        assert_eq!("check me out\n", &src_string[lines.range(0)]);
        assert_eq!("I'm some lox code\r\n", &src_string[lines.range(1)]);
        assert_eq!("for real", &src_string[lines.range(2)]);
    }

    #[test]
    fn line_number_of_byte_index() {
        let src_string = "check me out\nI'm some lox code\r\nfor real\n";
        let lines = super::SourceLines::new(&src_string);

        assert_eq!(lines.line_number_of_byte_index(0).unwrap(), 0);
        assert_eq!(lines.line_number_of_byte_index(3).unwrap(), 0);
        assert_eq!(lines.line_number_of_byte_index(11).unwrap(), 0);
        assert_eq!(lines.line_number_of_byte_index(12).unwrap(), 0);
        assert_eq!(lines.line_number_of_byte_index(13).unwrap(), 1);
        assert_eq!(lines.line_number_of_byte_index(31).unwrap(), 1);
        assert_eq!(lines.line_number_of_byte_index(32).unwrap(), 2);
        assert_eq!(lines.line_number_of_byte_index(33).unwrap(), 2);

        // this seems like it should work, but actually because the last "line" is
        // completely empty, no index is actually "contained" in it. in other words,
        // there's no characters on the line
        //~ assert_eq!(lines.line_number_of_byte_index(41).unwrap(), 3);

        // not sure what implications this has for pointing one past the end of the string/line
    }
}

*/
