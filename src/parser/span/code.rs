use parser::Span;
use parser::Span::Code;
use regex::Regex;

pub fn parse_code(text: &str) -> Option<(Span, usize)> {
    lazy_static! {
        static ref CODE_SINGLE: Regex = Regex::new(r"^`(?P<text>.+?)`").unwrap();
        static ref CODE_DOUBLE: Regex = Regex::new(r"^``(?P<text>.+?)``").unwrap();
    }

    if CODE_DOUBLE.is_match(text) {
        let caps = CODE_DOUBLE.captures(text).unwrap();
        let t = caps.name("text").unwrap().as_str();
        return Some((Code(t), t.len() + 4));
    } else if CODE_SINGLE.is_match(text) {
        let caps = CODE_SINGLE.captures(text).unwrap();
        let t = caps.name("text").unwrap().as_str();
        return Some((Code(t), t.len() + 2));
    }
    None
}

#[test]
fn finds_code() {
    assert_eq!(
        parse_code("`testing things` test"),
        Some((Code("testing things"), 16))
    );

    assert_eq!(
        parse_code("``testing things`` test"),
        Some((Code("testing things"), 18))
    );

    assert_eq!(
        parse_code("``testing things`` things`` test"),
        Some((Code("testing things"), 18))
    );

    assert_eq!(
        parse_code("`w` testing things test"),
        Some((Code("w"), 3))
    );

    assert_eq!(
        parse_code("`w`` testing things test"),
        Some((Code("w"), 3))
    );

    assert_eq!(
        parse_code("``w`` testing things test"),
        Some((Code("w"), 5))
    );

    assert_eq!(
        parse_code("``w``` testing things test"),
        Some((Code("w"), 5))
    );
}

#[test]
fn no_false_positives() {
    assert_eq!(parse_code("`` testing things test"), None);
    assert_eq!(parse_code("` test"), None);
}

#[test]
fn no_early_matching() {
    assert_eq!(parse_code("were ``testing things`` test"), None);
    assert_eq!(parse_code("were `testing things` test"), None);
}
