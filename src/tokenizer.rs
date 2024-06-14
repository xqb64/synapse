use logos::Logos;
#[derive(Debug, Clone, PartialEq, Default)]
pub enum TokenizerError {
    #[default]
    Other,
}

#[derive(Logos, Debug, PartialEq, Clone, Copy, Default)]
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

    #[token("for")]
    For,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("struct")]
    Struct,

    #[token("impl")]
    Impl,

    #[token("use")]
    Use,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    #[token("+")]
    Plus,

    #[token("+=")]
    PlusEqual,

    #[token("-")]
    Minus,

    #[token("-=")]
    MinusEqual,

    #[token("*")]
    Star,

    #[token("*=")]
    StarEqual,

    #[token("/")]
    Slash,

    #[token("/=")]
    SlashEqual,

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

    #[token(".")]
    Dot,

    #[token(":")]
    Colon,

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

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token(",")]
    Comma,

    #[token("%")]
    Percent,

    #[token("%=")]
    PercentEqual,

    #[token("&")]
    Ampersand,

    #[token("&&")]
    DoubleAmpersand,

    #[token("&=")]
    AmpersandEqual,

    #[token("|")]
    Pipe,

    #[token("||")]
    DoublePipe,

    #[token("|=")]
    PipeEqual,

    #[token("^")]
    Caret,

    #[token("^=")]
    CaretEqual,

    #[token("~")]
    Tilde,

    #[token(">>")]
    GreaterGreater,

    #[token(">>=")]
    GreaterGreaterEqual,

    #[token("<<")]
    LessLess,

    #[token("<<=")]
    LessLessEqual,

    #[token("->")]
    Arrow,

    /* It is not possible to use capture groups.
     *
     * https://github.com/maciejhirsz/logos/issues/327
     */
    #[regex(r#""[^\n"]*""#, |lex| { let s = lex.slice(); &s[1..s.len() - 1] })]
    String(&'src str),

    #[regex("[a-zA-Z_]+")]
    Identifier(&'src str),

    #[regex(r"[0-9]+(\.[0-9]+)?")]
    Number(&'src str),

    #[default]
    Error,
}

impl<'src> Token<'src> {
    pub fn get_value(&self) -> &'src str {
        match self {
            Token::Identifier(ident) => ident,
            Token::String(s) => s,
            Token::Number(n) => n,
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Tokenizer<'src> {
    pub lexer: logos::Lexer<'src, Token<'src>>,
}

impl<'src> Tokenizer<'src> {
    pub fn new(src: &'src str) -> Tokenizer<'src> {
        Tokenizer {
            lexer: Token::lexer(&src),
        }
    }

    pub fn get_lexer(&self) -> &logos::Lexer<'src, Token<'src>> {
        &self.lexer
    }
}

impl<'src> Iterator for Tokenizer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(Ok(r)) => Some(r),
            Some(Err(_)) => Some(Token::Error),
            None => None,
        }
    }
}
