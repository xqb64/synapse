use logos::Logos;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum TokenizerError {
    #[default]
    Other,
}

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error = TokenizerError)]
pub enum Token<'src> {
    #[token("print")]
    Print,

    #[token("fn")]
    Fn,

    #[token("return")]
    Return,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("while")]
    While,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("=")]
    Equal,

    #[token("!")]
    Bang,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("<=")]
    LessEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("!=")]
    BangEqual,

    #[token("==")]
    DoubleEqual,

    #[token("++")]
    PlusPlus,

    #[token(";")]
    Semicolon,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(",")]
    Comma,

    #[regex(r#""[^\n"]*""#, |lex| lex.slice())]
    String(&'src str),

    #[regex("[a-zA-Z_]+")]
    Identifier(&'src str),

    #[regex(r"[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse().ok())]
    Number(f64),
}

macro_rules! tokenizer_error {
    ($msg:expr) => {{
        eprintln!("tokenizer error: {}", $msg);
        std::process::exit(1);
    }};
}

pub struct Tokenizer<'src> {
    lexer: logos::Lexer<'src, Token<'src>>,
}

impl<'src> Tokenizer<'src> {
    pub fn new(src: &'src str) -> Tokenizer<'src> {
        Tokenizer {
            lexer: Token::lexer(src),
        }
    }
}

impl<'src> Iterator for Tokenizer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(Ok(r)) => Some(r),
            Some(Err(e)) => {
                tokenizer_error!(format!("Got unexpected token: {:?}", e));
            }
            None => None,
        }
    }
}
