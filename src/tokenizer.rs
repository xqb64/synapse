use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Print,
    Number,
    Plus,
    Minus,
    Star,
    Slash,
    String,
    Semicolon,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'source> {
    pub kind: TokenKind,
    pub value: &'source str,
}

impl<'source> Token<'source> {
    fn new(kind: TokenKind, value: &'source str) -> Token<'source> {
        Token { kind, value }
    }
}

pub struct Tokenizer<'source> {
    src: &'source str,
    start: usize,
}

impl<'source> Iterator for Tokenizer<'source> {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        let re_keyword = r"?P<keyword>print";
        let re_individual = r"?P<individual>[-+*/;]";
        let re_number = r"?P<number>[-+]?\d+(\.\d+)?";
        let re_string = r#""(?P<string>[^\n"]*)""#;

        let r = Regex::new(
            format!(
                "({})|({})|({})|({})",
                re_keyword, re_individual, re_number, re_string,
            )
            .as_str(),
        )
        .unwrap();

        let token = match r.captures_at(self.src, self.start) {
            Some(captures) => {
                if let Some(m) = captures.name("keyword") {
                    self.start = m.end();
                    match m.as_str() {
                        "print" => Token::new(TokenKind::Print, "print"),
                        _ => unreachable!(),
                    }
                } else if let Some(m) = captures.name("individual") {
                    self.start = m.end();
                    match m.as_str() {
                        "+" => Token::new(TokenKind::Plus, "+"),
                        "-" => Token::new(TokenKind::Minus, "-"),
                        "*" => Token::new(TokenKind::Star, "*"),
                        "/" => Token::new(TokenKind::Slash, "/"),
                        ";" => Token::new(TokenKind::Semicolon, ";"),
                        _ => unreachable!(),
                    }
                } else if let Some(m) = captures.name("number") {
                    self.start = m.end();
                    Token::new(TokenKind::Number, m.as_str())
                } else if let Some(m) = captures.name("string") {
                    self.start = m.end();
                    Token::new(TokenKind::String, m.as_str())
                } else {
                    return None;
                }
            }
            None => return None,
        };

        Some(token)
    }
}

impl<'source> Tokenizer<'source> {
    pub fn new(src: &'source str) -> Tokenizer<'source> {
        Tokenizer { src, start: 0 }
    }
}
