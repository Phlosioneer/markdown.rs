use parser::span::parse_spans_with_buffer;
use parser::Block;
use parser::Block::Paragraph;
use parser::Span::{Break, Text};

mod atx_header;
mod blockquote;
mod code_block;
mod hr;
mod link_reference;
mod ordered_list;
mod setext_header;
mod unordered_list;
use self::atx_header::parse_atx_header;
use self::blockquote::parse_blockquote;
use self::code_block::parse_code_block;
use self::hr::parse_hr;
use self::link_reference::parse_link_reference;
use self::ordered_list::parse_ordered_list;
use self::setext_header::parse_setext_header;
use self::unordered_list::parse_unordered_list;

pub fn parse_blocks(md: &str) -> Vec<Block> {
    let mut t = vec![];
    let lines: Vec<&str> = md.lines().collect();
    let mut blocks = Vec::with_capacity(lines.len());
    let mut i = 0;
    while i < lines.len() {
        match parse_block(&lines[i..lines.len()]) {
            // if a block is found
            Some((block, consumed_lines)) => {
                // the current paragraph has ended,
                // push it to our blocks
                if !t.is_empty() {
                    blocks.push(Paragraph(t));
                    t = Vec::new();
                }
                blocks.push(block);
                i += consumed_lines;
            }
            // no known element, let's make this a paragraph
            None => {
                // empty linebreak => new paragraph
                if lines[i].is_empty() && !t.is_empty() {
                    blocks.push(Paragraph(t));
                    t = Vec::new();
                }

                let span_start_index = t.len();
                parse_spans_with_buffer(lines[i], &mut t);
                let last_span_before_call = if span_start_index == 0 {
                    None
                } else {
                    t.get(span_start_index - 1)
                };
                let first_span_from_call = t.get(span_start_index);

                // add a newline between linebreaks
                // except when we have a break element or nothing
                match (last_span_before_call, first_span_from_call) {
                    (Some(&Break), _) => {}
                    (_, None) => {}
                    (None, _) => {}
                    _ => t.insert(span_start_index, Text("\n".to_owned())),
                }

                i += 1;
            }
        }
    }
    if !t.is_empty() {
        blocks.push(Paragraph(t));
    }
    blocks
}

fn parse_block(lines: &[&str]) -> Option<(Block, usize)> {
    match lines[0].chars().nth(0)? {
        '#' => parse_atx_header(lines),
        '=' => parse_hr(lines),
        '-' => parse_hr(lines).or_else(||parse_unordered_list(lines)),
        '`' => parse_code_block(lines),
        '>' => parse_blockquote(lines),
        '0'..='9' => parse_ordered_list(lines),
        '[' => parse_link_reference(lines),
        '*' => parse_unordered_list(lines),
        
        // Slow path
        '\t' => parse_code_block(lines)
            .or_else(||parse_unordered_list(lines))
            .or_else(||parse_ordered_list(lines))
            .or_else(||parse_link_reference(lines)),

        ' ' => {
            // If there are less than 4 spaces, this can't be a code block. Use that
            // to speed up the matching.
            let mut iter = lines[0].chars().take(4).peekable();
            while iter.peek() == Some(&' ') { iter.next(); }
            
            if let Some(c) = iter.peek() {
                match c {
                    '-' | '*' => parse_unordered_list(lines),
                    '[' => parse_link_reference(lines),
                    '0'..='9' => parse_ordered_list(lines),
                    _ => None,
                }
            } else {
                // Slow path
                parse_code_block(lines)
                    .or_else(||parse_unordered_list(lines))
                    .or_else(||parse_ordered_list(lines))
                    .or_else(||parse_link_reference(lines))
            }
        },
        _ => None
    }.or_else(|| {
        if lines.len() > 1 {
            if let Some(c1) = lines[1].chars().nth(0) {
                if c1 == '-' || c1 == '=' {
                    return parse_setext_header(lines);
                }
            }
        }
        None
    })
}

#[cfg(test)]
mod test {
    use super::parse_blocks;
    use parser::Block::{Blockquote, CodeBlock, Header, Hr, Paragraph};
    use parser::Span::Text;

    #[test]
    fn finds_atx_header() {
        assert_eq!(
            parse_blocks("### Test"),
            vec![Header(vec![Text("Test".to_owned())], 3)]
        );
    }

    #[test]
    fn finds_setext_header() {
        assert_eq!(
            parse_blocks("Test\n-------"),
            vec![Header(vec![Text("Test".to_owned())], 2)]
        );
        assert_eq!(
            parse_blocks("Test\n======="),
            vec![Header(vec![Text("Test".to_owned())], 1)]
        );
    }

    #[test]
    fn finds_hr() {
        assert_eq!(parse_blocks("-------"), vec![Hr]);
        assert_eq!(parse_blocks("======="), vec![Hr]);
    }

    #[test]
    fn finds_code_block() {
        assert_eq!(
            parse_blocks("    this is code\n    and this as well"),
            vec![CodeBlock(None, "this is code\nand this as well".to_owned())]
        );

        assert_eq!(
            parse_blocks("```\nthis is code\nand this as well\n```"),
            vec![CodeBlock(
                Some(String::new()),
                "this is code\nand this as well".to_owned()
            )]
        );
    }

    #[test]
    fn finds_blockquotes() {
        assert_eq!(
            parse_blocks("> One Paragraph\n>\n> ## H2 \n>\n"),
            vec![Blockquote(vec![
                Paragraph(vec![Text("One Paragraph".to_owned())]),
                Header(vec![Text("H2".to_owned())], 2)
            ])]
        );

        assert_eq!(
            parse_blocks("> One Paragraph\n>\n> > Another blockquote\n>\n"),
            vec![Blockquote(vec![
                Paragraph(vec![Text("One Paragraph".to_owned())]),
                Blockquote(vec![Paragraph(vec![Text("Another blockquote".to_owned())])])
            ])]
        );

        assert_eq!(
            parse_blocks("> > One Paragraph\n> >\n> > Another blockquote\n>\n"),
            vec![Blockquote(vec![Blockquote(vec![
                Paragraph(vec![Text("One Paragraph".to_owned())]),
                Paragraph(vec![Text("Another blockquote".to_owned())])
            ])])]
        );

        assert_eq!(
            parse_blocks("> One Paragraph, just > text \n>\n"),
            vec![Blockquote(vec![Paragraph(vec![Text(
                "One Paragraph, just > text".to_owned()
            )])])]
        );

        assert_eq!(
            parse_blocks("> One Paragraph\n>\n> just > text \n>\n"),
            vec![Blockquote(vec![
                Paragraph(vec![Text("One Paragraph".to_owned())]),
                Paragraph(vec![Text("just > text".to_owned())])
            ])]
        );
    }
}
