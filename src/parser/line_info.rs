
use super::{Block, Span, ListItem};
use std::cmp::{min, max};
use std::convert::TryFrom;

/// Trait to reduce code duplication for arrays of spans, listitems, blocks,
/// and strings.
pub trait IntoLineInfo {
    fn into_line_info(&self, original: &str) -> Option<LineInfo>;
}

/// Struct with information about the start and end of a block or span
/// in the original file or string.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone)]
pub struct LineInfo {
    pub start_line: usize,
    pub start_char: usize,
    pub end_line: usize,
    pub end_char: usize
}

impl LineInfo {
    fn join(mut self, other: LineInfo) -> Self {
        if self.start_line == other.start_line {
            self.start_char = min(self.start_char, other.start_char);
        } else if self.start_line > other.start_line {
            self.start_line = other.start_line;
            self.start_char = other.start_char;
        }

        if self.end_line == other.end_line {
            self.end_char = max(self.end_char, other.end_char);
        } else if self.end_line < other.end_line {
            self.end_line = other.end_line;
            self.end_char = other.end_char;
        }

        self
    }

    fn try_join(this: Option<LineInfo>, other: Option<LineInfo>) -> Option<LineInfo> {
        match (this, other) {
            (Some(this), Some(other)) => Some(this.join(other)),
            (None, other) => other,
            (this, None) => this
        }
    }
}

impl<'a> IntoLineInfo for Block<'a> {
    fn into_line_info(&self, original: &str) -> Option<LineInfo> {
        use Block::*;

        match self {
            Header(spans, _) |
            Paragraph(spans) => spans.into_line_info(original),
            Blockquote(blocks) => blocks.into_line_info(original),
            CodeBlock(language, lines) => {
                LineInfo::try_join(
                    lines.into_line_info(original),
                    language.map(|l| l.into_line_info(original)).flatten())
            },
            LinkReference(_, url, title) => {
                LineInfo::try_join(
                    url.into_line_info(original),
                    title.map(|l| l.into_line_info(original)).flatten())
            },
            OrderedList(items, _) |
            UnorderedList(items) => items.into_line_info(original),
            Hr => None,
        }
    }
}

impl<'a> IntoLineInfo for Span<'a> {
    fn into_line_info(&self, original: &str) -> Option<LineInfo> {
        use Span::*;

        match self {
            Break | Literal(_) => None,
            Text(t) | Code(t) => t.into_line_info(original),
            Link(spans, url, title) => {
                LineInfo::try_join(
                    LineInfo::try_join(
                        spans.into_line_info(original),
                        url.into_line_info(original)),
                    title.map(|t| t.into_line_info(original)).flatten())
            },
            RefLink(spans, _) |
            Emphasis(spans) |
            Strong(spans) => spans.into_line_info(original),
            Image(text, url, title) => {
                LineInfo::try_join(
                    LineInfo::try_join(
                        text.into_line_info(original),
                        url.into_line_info(original)),
                    title.map(|t| t.into_line_info(original)).flatten())
            }
        }
    }
}

impl<'a> IntoLineInfo for ListItem<'a> {

    fn into_line_info(&self, original: &str) -> Option<LineInfo> {
        use ListItem::*;

        match self {
            Simple(spans) => (&spans).into_line_info(original),
            Paragraph(blocks) => blocks.into_line_info(original)
        }
    }
}

impl<T: IntoLineInfo> IntoLineInfo for Vec<T> {
    fn into_line_info(&self, original: &str) -> Option<LineInfo> {
        // Find the first valid LineInfo
        let mut first_valid = None;
        for s in self.iter() {
            first_valid = s.into_line_info(original);
            if first_valid.is_some() {
                break;
            }
        }
        let first_valid = first_valid?;

        // Find the last valid LineInfo.
        let mut last_valid = None;
        for s in self.iter().rev() {
            last_valid = s.into_line_info(original);
            if last_valid.is_some() {
                break;
            }
        }
        let last_valid = last_valid.unwrap();

        Some(LineInfo::join(first_valid, last_valid))
    }
}
/*
    fn from_spans(original: &str, strs: &[Span]) -> Option<LineInfo> {
        // Find the first valid LineInfo
        let mut first_valid = None;
        for s in strs {
            first_valid = LineInfo::from_span(original, s);
            if first_valid.is_some() {
                break;
            }
        }
        let first_valid = first_valid?;

        // Find the last valid LineInfo.
        let mut last_valid = None;
        for s in strs.iter().rev() {
            last_valid = LineInfo::from_span(original, s);
            if last_valid.is_some() {
                break;
            }
        }
        let last_valid = last_valid.unwrap();

        Some(LineInfo::join(first_valid, last_valid))
    }

    fn from_strs(original: &str, strs: &[&str]) -> Option<LineInfo> {
        // Find the first valid LineInfo
        let mut first_valid = None;
        for s in strs {
            first_valid = LineInfo::from_str(original, s);
            if first_valid.is_some() {
                break;
            }
        }
        let first_valid = first_valid?;

        // Find the last valid LineInfo.
        let mut last_valid = None;
        for s in strs.iter().rev() {
            last_valid = LineInfo::from_str(original, s);
            if last_valid.is_some() {
                break;
            }
        }
        let last_valid = last_valid.unwrap();

        Some(LineInfo::join(first_valid, last_valid))
    }
*/

impl<'a> IntoLineInfo for &'a str {
    /// Unsafe contract enforced by {Block, ListItem, Span}::get_line_info.
    fn into_line_info(&self, original: &str) -> Option<LineInfo> {
        let original_range = original.as_bytes().as_ptr_range();
        let substr_range = self.as_bytes().as_ptr_range();
        let start_offset;
        if !original_range.contains(&substr_range.start) {
            return None;
        }
        unsafe {
            assert_eq!(original_range.contains(&substr_range.start), true);
            start_offset = substr_range.start.offset_from(original_range.start);
        };
        let start_offset = usize::try_from(start_offset).unwrap();
        assert_eq!(&original[start_offset..start_offset + self.len()], *self);

        // Count the number of newlines between the start of the original and
        // the start of the substring.
        let start_line = original[..start_offset].matches('\n').count();

        // Count the number of characters since the last newline.
        // NOT the same as the number of bytes!
        let start_char = if start_line == 0 {
            original[..start_offset].chars().count()
        } else {
            let last_newline_index = original[..start_offset].rfind('\n').unwrap();
            original[last_newline_index + 1..start_offset].chars().count()
        };

        // Count the number of newlines in the substring.
        let substring_lines = self.matches('\n').count();
        let end_line = start_line + substring_lines;

        // Count the number of characters since the last newline.
        let end_char = if substring_lines == 0 {
            start_char + self.chars().count()
        } else {
            let last_newline_index = self.rfind('\n').unwrap();
            self[last_newline_index + 1..].chars().count()
        };

        Some(LineInfo { start_line, start_char, end_line, end_char })
    }
}
