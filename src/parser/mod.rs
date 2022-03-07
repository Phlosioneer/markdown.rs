
mod block;
mod span;
mod line_info;

pub use self::line_info::LineInfo;
use self::line_info::IntoLineInfo;

pub fn parse(md: &str) -> Vec<Block> {
    block::parse_blocks(md)
}

/// The style for ordered list numerals
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OrderedListType {
    /// ```text
    /// 1. First item
    /// 2. Second item
    /// 3. Third item
    /// ```
    Numeric,
    /// ```text
    /// a. First item
    /// b. Second item
    /// c. Third item
    /// ```
    Lowercase,
    /// ```text
    /// A. First item
    /// B. Second item
    /// C. Third item
    /// ```
    Uppercase,
    /// ```text
    /// i. First item
    /// ii. Second item
    /// iii. Third item
    /// iv. Fourth item
    /// ```
    LowercaseRoman,
    /// ```text
    /// I. First item
    /// II. Second item
    /// III. Third item
    /// IV. Fourth item
    /// ```
    UppercaseRoman,
}

impl OrderedListType {
    fn from_str(type_str: &str) -> OrderedListType {
        match type_str {
            "a" => OrderedListType::Lowercase,
            "A" => OrderedListType::Uppercase,
            "i" => OrderedListType::LowercaseRoman,
            "I" => OrderedListType::UppercaseRoman,
            _ => OrderedListType::Numeric,
        }
    }

    /// Converts the ordered list type into the corresponding <ol> "type"
    /// attribute value.
    pub fn to_html_type(&self) -> &'static str {
        match self {
            OrderedListType::Lowercase => "a",
            OrderedListType::Uppercase => "A",
            OrderedListType::LowercaseRoman => "i",
            OrderedListType::UppercaseRoman => "I",
            OrderedListType::Numeric => "1",
        }
    }
}

/// Blocks are parts of the markdown file separated by one or more blank lines.
#[derive(Debug, PartialEq, Clone)]
pub enum Block<'a> {
    /// Lines that start with #. The first field is the tokens in the title string, and
    /// the second field is the level of the header.
    /// 
    /// For ATX-style headers that start with "#", the level of a header starts at 1 with
    /// one "#", then 2 with "##", etc. Only levels up to 6 are supported.
    /// 
    /// For Setext-style headers that are underlined with characters, underlining with "="
    /// corresponds to level 1 and underlining with "-" corresponds to level 2.
    Header(Vec<Span<'a>>, usize),
    
    /// Sections that are plain text, and don't fit into any other section.
    Paragraph(Vec<Span<'a>>),
    
    /// Sections indented by >. Nested blockquotes are possible.
    Blockquote(Vec<Block<'a>>),
    
    /// Multi-line code beginning with triple-backticks (\`\`\`), intented with tabs, or indented
    /// with four spaces. The first field is the language specifier (if any), and the second field
    /// is the lines within the code block.
    CodeBlock(Option<&'a str>, Vec<&'a str>),
    
    /// This is the second half of a reference-style link. The first field is a the label, the second
    /// field is the URL, and the third field is a title for the link (if any).
    /// 
    /// Labels are supposed to be case-insensitive, so they are saved in all-lowercase format.
    LinkReference(String, &'a str, Option<&'a str>),
    
    /// An ordered list, with a numbering scheme given by OrderedListType. Nested lists are not currently
    /// supported.
    OrderedList(Vec<ListItem<'a>>, OrderedListType),

    /// An unordered list. Nested lists are not currently supported.
    UnorderedList(Vec<ListItem<'a>>),

    /// A horizontal rule (or divider).
    Hr,
}

impl<'a> Block<'a> {
    /// Combine the lines of a codeblock with newlines. They're stored as
    /// separate lines to avoid the allocation, this method performs that
    /// allocation.
    pub fn complete_code_block(&self) -> String {
        match self {
            Block::CodeBlock(_, lines) => lines.join("\n"),
            _ => panic!("Not a code block")
        }
    }

    /// Convert to a version without lifetimes. The conversion combines code
    /// block lines into a single string. The conversion will also join
    /// any adjacent text spans and literal spans together.
    fn to_owned(&self) -> OwnedBlock {
        match self {
            Block::Header(spans, level) => OwnedBlock::Header(
                to_owned_spans(spans),
                *level
            ),
            Block::Paragraph(spans) => OwnedBlock::Paragraph(
                to_owned_spans(spans)
            ),
            Block::Blockquote(blocks) => OwnedBlock::Blockquote(
                blocks.iter().map(|b| b.to_owned()).collect()
            ),
            Block::CodeBlock(language, lines) => OwnedBlock::CodeBlock(
                language.map(|s| s.to_owned()),
                lines.join("\n")
            ),
            Block::LinkReference(label, url, title) => OwnedBlock::LinkReference(
                label.clone(),
                url.to_string(),
                title.map(|s| s.to_string())
            ),
            Block::OrderedList(items, list_type) => OwnedBlock::OrderedList(
                items.iter().map(|i| i.to_owned()).collect(),
                *list_type
            ),
            Block::UnorderedList(items) => OwnedBlock::UnorderedList(
                items.iter().map(|i| i.to_owned()).collect()
            ),
            Block::Hr => OwnedBlock::Hr,
        }
    }

    /// This function will produce Undefined Behavior unless `original`
    /// comes from the EXACT SAME allocated object as the string slice that
    /// was passed to the parser.
    /// 
    /// This function may produce junk results unless `original` is a slice
    /// over the same indices as the slice passed to the parser, or it may
    /// return None.
    /// 
    /// This function will return None if the block or span only contains
    /// static strings.
    pub unsafe fn get_line_info(&self, original: &str) -> Option<LineInfo> {
        self.into_line_info(original)
    }
}

/// Single item inside of a list.
#[derive(Debug, PartialEq, Clone)]
pub enum ListItem<'a> {
    /// Simple single-line item.
    Simple(Vec<Span<'a>>),
    /// Multi-line item. May contain nearly anything.
    Paragraph(Vec<Block<'a>>),
}

impl<'a> ListItem<'a> {
    /// Convert to a version without lifetimes. The conversion combines code
    /// block lines into a single string. The conversion will also join
    /// any adjacent text spans and literal spans together.
    pub fn to_owned(&self) -> OwnedListItem {
        match self {
            ListItem::Simple(spans) => OwnedListItem::Simple(
                to_owned_spans(spans)
            ),
            ListItem::Paragraph(blocks) => OwnedListItem::Paragraph(
                blocks.iter().map(|b| b.to_owned()).collect()
            )
        }
    }

    /// This function will produce Undefined Behavior unless `original`
    /// comes from the EXACT SAME allocated object as the string slice that
    /// was passed to the parser.
    /// 
    /// This function may produce junk results unless `original` is a slice
    /// over the same indices as the slice passed to the parser, or it may
    /// return None.
    /// 
    /// This function will return None if the block or span only contains
    /// static strings.
    pub unsafe fn get_line_info(&self, original: &str) -> Option<LineInfo> {
        self.into_line_info(original)
    }
}

/// Formatting elements within text.
#[derive(Debug, PartialEq, Clone)]
pub enum Span<'a> {
    /// Line break. This is considered part of inline text processing. It's up to the formatter
    /// or interpreter to decide whether lines with linebreaks are different from adjacent
    /// paragraph blocks.
    Break,

    /// Plain unformatted text.
    Text(&'a str),

    /// Preformatted text.
    Code(&'a str),

    /// Literals should be treated the same as Text. Due to technical restrictions they are stored
    /// separately from other text.
    Literal(char),

    /// A self-contained link. The first field is the text shown to the user, which may have additional
    /// formatting. The second field is the URL for the link. Then the third field is a title for the
    /// link, if provided.
    Link(Vec<Span<'a>>, &'a str, Option<&'a str>),
    
    /// The first part of a reference-style link. Reference-style links move the URL from inline with
    /// the link, to somewhere else in the document. The first field is the text shown to the user, which
    /// may have additional formatting. The second field is the label for the link. Link labels are
    /// case-insensitive, so the label is converted to lowercase first.
    RefLink(Vec<Span<'a>>, String),

    /// An embedded image. The first field is the alt-text of the image, the second field is the url of
    /// the image, and the third field is a title for the image (if any).
    /// 
    /// The interpretation of the image title is unclear and varies between parsers.
    Image(&'a str, &'a str, Option<&'a str>),

    /// Italic text.
    Emphasis(Vec<Span<'a>>),

    /// Bold text.
    Strong(Vec<Span<'a>>),
}

impl<'a> Span<'a> {
    /// Convert to a version without lifetimes. The conversion will also join
    /// any adjacent text spans and literal spans together.
    pub fn to_owned(&self) -> OwnedSpan {
        match self {
            Span::Break => OwnedSpan::Break,
            Span::Text(text) => OwnedSpan::Text(text.to_string()),
            Span::Code(code) => OwnedSpan::Code(code.to_string()),
            Span::Literal(c) => OwnedSpan::Text(c.to_string()),
            Span::Link(spans, url, title) => OwnedSpan::Link(
                to_owned_spans(spans),
                url.to_string(),
                title.map(|s| s.to_string())
            ),
            Span::RefLink(spans, label) => OwnedSpan::RefLink(
                to_owned_spans(spans),
                label.clone()
            ),
            Span::Image(text, url, title) => OwnedSpan::Image(
                text.to_string(),
                url.to_string(),
                title.map(|s| s.to_string())
            ),
            Span::Emphasis(spans) => OwnedSpan::Emphasis(
                to_owned_spans(spans)
            ),
            Span::Strong(spans) => OwnedSpan::Strong(
                to_owned_spans(spans)
            )
        }
    }

    /// This function will produce Undefined Behavior unless `original`
    /// comes from the EXACT SAME allocated object as the string slice that
    /// was passed to the parser.
    /// 
    /// This function may produce junk results unless `original` is a slice
    /// over the same indices as the slice passed to the parser, or it may
    /// return None.
    /// 
    /// This function will return None if the block or span only contains
    /// static strings.
    pub unsafe fn get_line_info(&self, original: &str) -> Option<LineInfo> {
        self.into_line_info(original)
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(missing_docs)]
pub enum OwnedBlock {
    Header(Vec<OwnedSpan>, usize),
    Paragraph(Vec<OwnedSpan>),
    Blockquote(Vec<OwnedBlock>),
    CodeBlock(Option<String>, String),
    LinkReference(String, String, Option<String>),
    OrderedList(Vec<OwnedListItem>, OrderedListType),
    UnorderedList(Vec<OwnedListItem>),
    Hr
}

#[derive(Debug, PartialEq, Clone)]
#[allow(missing_docs)]
pub enum OwnedListItem {
    Simple(Vec<OwnedSpan>),
    Paragraph(Vec<OwnedBlock>)
}

#[derive(Debug, PartialEq, Clone)]
#[allow(missing_docs)]
pub enum OwnedSpan {
    Break,
    Text(String),
    Code(String),
    Link(Vec<OwnedSpan>, String, Option<String>),
    RefLink(Vec<OwnedSpan>, String),
    Image(String, String, Option<String>),
    Emphasis(Vec<OwnedSpan>),
    Strong(Vec<OwnedSpan>),
}

fn to_owned_spans(spans: &[Span]) -> Vec<OwnedSpan> {
    let mut ret = Vec::with_capacity(spans.len());
    for span in spans {
        if let Some(OwnedSpan::Text(ref mut text)) = ret.last_mut()  {
            match span {
                Span::Text(extra_text) => text.push_str(extra_text),
                Span::Literal(c) => text.push(*c),
                _ => ret.push(span.to_owned())
            }
        } else {
            ret.push(span.to_owned())
        }
    }
    ret
}

macro_rules! variant_helper_impls {
    ($name: ident, $is_name:ident, $matcher:pat, $ret:tt, $ret_type:ty) => {
        #[doc="Returns true if this enum is in the "]
        #[doc= stringify!($name)]
        #[doc=" variant."]
        #[allow(unused)]
        #[allow(unused_parens)]
        pub fn $is_name(&self) -> bool { matches!(self, $matcher) }

        /// Returns the contents of the variant, if able.
        #[allow(unused_parens)]
        pub fn $name(&self) -> Result<$ret_type, String> {
            if let $matcher = self {
                Ok($ret)
            } else {
                Err(format!("Wrong variant {}: {:?}", stringify!($name), self))
            }
        }
    };
    ($is_name: ident, $matcher:pat) => {
        #[doc="Returns true if this enum is in the "]
        #[doc= stringify!($name)]
        #[doc=" variant."]
        #[allow(unused)]
        pub fn $is_name(&self) -> bool { matches!(self, $matcher) }
    }
}

impl<'a> Block<'a> {
    variant_helper_impls!(header, is_header,
        Block::Header(spans, level),
        (spans, *level), (&[Span<'a>], usize));
    variant_helper_impls!(paragraph, is_paragraph,
        Block::Paragraph(spans),
        spans, &[Span<'a>]);
    variant_helper_impls!(block_quote, is_block_quote,
        Block::Blockquote(blocks),
        blocks, &[Block<'a>]);
    variant_helper_impls!(code_block, is_code_block,
        Block::CodeBlock(lang, lines),
        (lang.clone(), lines), (Option<&'a str>, &[&'a str]));
    variant_helper_impls!(link_reference, is_link_reference,
        Block::LinkReference(id, url, title),
        (id, url, title.clone()), (&str, &'a str, Option<&'a str>));
    variant_helper_impls!(ordered_list, is_ordered_list,
        Block::OrderedList(items, list_type),
        (items, *list_type), (&[ListItem<'a>], OrderedListType));
    variant_helper_impls!(is_hr, Block::Hr);
    variant_helper_impls!(unordered_list, is_unordered_list,
        Block::UnorderedList(items),
        items, &[ListItem<'a>]);
}

impl<'a> ListItem<'a> {
    variant_helper_impls!(simple, is_simple,
        ListItem::Simple(spans), spans, &[Span<'a>]);
    variant_helper_impls!(paragraph, is_paragraph,
        ListItem::Paragraph(blocks), blocks, &[Block<'a>]);
}

impl<'a> Span<'a> {
    variant_helper_impls!(is_break, Span::Break);
    variant_helper_impls!(text, is_text,
        Span::Text(text),
        text, &'a str);
    variant_helper_impls!(code, is_code,
        Span::Code(code),
        code, &'a str);
    variant_helper_impls!(literal, is_literal,
        Span::Literal(c),
        (*c), char);
    variant_helper_impls!(link, is_link,
        Span::Link(spans, url, title),
        (spans, url, title.clone()), (&[Span<'a>], &'a str, Option<&'a str>));
    variant_helper_impls!(ref_link, is_ref_link,
        Span::RefLink(spans, id),
        (spans, id), (&[Span<'a>], &str));
    variant_helper_impls!(image, is_image,
        Span::Image(text, url, title),
        (text, url, title.clone()), (&'a str, &'a str, Option<&'a str>));
    variant_helper_impls!(emphasis, is_emphasis,
        Span::Emphasis(spans),
        spans, &[Span<'a>]);
    variant_helper_impls!(strong, is_strong,
        Span::Strong(spans),
        spans, &[Span<'a>]);
}

impl OwnedBlock {
    variant_helper_impls!(header, is_header,
        OwnedBlock::Header(spans, level),
        (spans, *level), (&[OwnedSpan], usize));
    variant_helper_impls!(paragraph, is_paragraph,
        OwnedBlock::Paragraph(spans),
        spans, &[OwnedSpan]);
    variant_helper_impls!(block_quote, is_block_quote,
        OwnedBlock::Blockquote(blocks),
        blocks, &[OwnedBlock]);
    variant_helper_impls!(code_block, is_code_block,
        OwnedBlock::CodeBlock(lang, lines),
        (lang.as_ref().map(|s| s.as_str()), lines), (Option<&str>, &str));
    variant_helper_impls!(link_reference, is_link_reference,
        OwnedBlock::LinkReference(id, url, title),
        (id, url, title.as_ref().map(|s| s.as_str())),
        (&str, &str, Option<&str>));
    variant_helper_impls!(ordered_list, is_ordered_list,
        OwnedBlock::OrderedList(items, list_type),
        (items, *list_type), (&[OwnedListItem], OrderedListType));
    variant_helper_impls!(is_hr, OwnedBlock::Hr);
    variant_helper_impls!(unordered_list, is_unordered_list,
        OwnedBlock::UnorderedList(items),
        items, &[OwnedListItem]);
}

impl OwnedListItem {
    variant_helper_impls!(simple, is_simple,
        OwnedListItem::Simple(spans), spans, &[OwnedSpan]);
    variant_helper_impls!(paragraph, is_paragraph,
        OwnedListItem::Paragraph(blocks), blocks, &[OwnedBlock]);
}

impl OwnedSpan {
    variant_helper_impls!(is_break, OwnedSpan::Break);
    variant_helper_impls!(text, is_text,
        OwnedSpan::Text(text),
        text, &str);
    variant_helper_impls!(code, is_code,
        OwnedSpan::Code(code),
        code, &str);
    variant_helper_impls!(link, is_link,
        OwnedSpan::Link(spans, url, title),
        (spans, url, title.as_ref().map(|s| s.as_str())),
        (&[OwnedSpan], &str, Option<&str>));
    variant_helper_impls!(ref_link, is_ref_link,
        OwnedSpan::RefLink(spans, id),
        (spans, id), (&[OwnedSpan], &str));
    variant_helper_impls!(image, is_image,
        OwnedSpan::Image(text, url, title),
        (text, url, title.as_ref().map(|s| s.as_str())),
        (&str, &str, Option<&str>));
    variant_helper_impls!(emphasis, is_emphasis,
        OwnedSpan::Emphasis(spans),
        spans, &[OwnedSpan]);
    variant_helper_impls!(strong, is_strong,
        OwnedSpan::Strong(spans),
        spans, &[OwnedSpan]);
}

#[test]
fn test_span_simplify() {
    assert_eq!(
        to_owned_spans(&vec![Span::Text("he"), Span::Text("llo")]),
        vec![OwnedSpan::Text("hello".to_string())]
    );
    assert_eq!(
        to_owned_spans(&vec![Span::Text("he"), Span::Literal('l'), Span::Text("lo")]),
        vec![OwnedSpan::Text("hello".to_string())]
    );
    assert_eq!(
        to_owned_spans(&vec![]),
        vec![]
    );
    assert_eq!(
        to_owned_spans(&vec![Span::Text("he"), Span::Text("llo")]),
        vec![OwnedSpan::Text("hello".to_string())]
    );
}

#[test]
fn test_line_numbers() {
    let test_str = "*foo* *bar*\n\nfoo *baz*\ntest\n\n> foobar\n> foobaz *blam*\n\n----".to_string();
    let tokens = parse(&test_str);

    assert_eq!(tokens, vec![
        Block::Paragraph(vec![
            Span::Emphasis(vec![Span::Text("foo")]),
            Span::Text(" "),
            Span::Emphasis(vec![Span::Text("bar")])
        ]),
        Block::Paragraph(vec![
            Span::Text("foo "),
            Span::Emphasis(vec![Span::Text("baz")]),
            Span::Text("\n"),
            Span::Text("test"),
        ]),
        Block::Blockquote(vec![
            Block::Paragraph(vec![
                Span::Text("foobar"),
                Span::Text("\n"),
                Span::Text("foobaz "),
                Span::Emphasis(vec![Span::Text("blam")])
            ])
        ]),
        Block::Hr
    ]);

    assert_eq!(
        tokens.into_line_info(&test_str),
        Some(LineInfo {
            start_line: 0, start_char: 1,
            end_line: 6, end_char: 14
        })
    );

    let foo = &tokens[0].paragraph().unwrap()[0]
        .emphasis().unwrap()[0];
    assert_eq!(
        foo.into_line_info(&test_str),
        Some(LineInfo {
            start_line: 0, start_char: 1,
            end_line: 0, end_char: 4
        })
    );
    
    let bar = &tokens[0].paragraph().unwrap()[2]
        .emphasis().unwrap()[0];
    assert_eq!(
        bar.into_line_info(&test_str),
        Some(LineInfo {
            start_line: 0, start_char: 7,
            end_line: 0, end_char: 10
        })
    );
    
    let strong_bar = &tokens[0].paragraph().unwrap()[2];
    assert_eq!(
        strong_bar.into_line_info(&test_str),
        Some(LineInfo {
            start_line: 0, start_char: 7,
            end_line: 0, end_char: 10
        })
    );
    
    let foo_baz_test = &tokens[1];
    assert_eq!(
        foo_baz_test.into_line_info(&test_str),
        Some(LineInfo {
            start_line: 2, start_char: 0,
            end_line: 3, end_char: 4
        })
    );

    let block_quote = &tokens[2];
    assert_eq!(
        block_quote.into_line_info(&test_str),
        Some(LineInfo {
            start_line: 5, start_char: 2,
            end_line: 6, end_char: 14
        })
    );

    let hr = &tokens[3];
    assert_eq!(hr.into_line_info(&test_str), None);

    let static_line_break = &tokens[1].paragraph().unwrap()[2];
    assert_eq!(static_line_break.into_line_info(&test_str), None);
}