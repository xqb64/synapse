use regex::Regex;
use std::{collections::VecDeque, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenKind {
    Print,
    Number,
    Plus,
    String,
    Semicolon,
}

#[derive(Debug, Clone, Copy)]
struct Token<'source> {
    kind: TokenKind,
    value: &'source str,
}

impl<'source> Token<'source> {
    fn new(kind: TokenKind, value: &'source str) -> Token<'source> {
        Token { kind, value }
    }
}

struct Tokenizer<'source> {
    src: &'source str,
    start: usize,
}

impl<'source> Iterator for Tokenizer<'source> {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        let re_keyword = r"?P<keyword>print";
        let re_individual = r"?P<individual>[+;]";
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
                        ";" => Token::new(TokenKind::Semicolon, ";"),
                        _ => unreachable!(),
                    }
                } else if let Some(m) = captures.name("number") {
                    self.start = m.end();
                    Token::new(
                        TokenKind::Number,
                        &self.src[self.start..self.start + m.as_str().chars().count()],
                    )
                } else if let Some(m) = captures.name("string") {
                    self.start = m.end();
                    Token::new(
                        TokenKind::String,
                        &self.src[self.start..self.start + m.as_str().chars().count()],
                    )
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
    fn new(src: &'source str) -> Tokenizer<'source> {
        Tokenizer { src, start: 0 }
    }
}

struct Parser<'source> {
    current: Option<Token<'source>>,
    previous: Option<Token<'source>>,
    tokens: VecDeque<Token<'source>>,
}

impl<'source> Parser<'source> {
    fn new() -> Self {
        Parser {
            current: None,
            previous: None,
            tokens: VecDeque::new(),
        }
    }

    fn parse(&mut self, tokens: VecDeque<Token<'source>>) -> Vec<Statement<'source>> {
        self.tokens = tokens;
        self.advance();
        let mut statements = vec![];
        while self.current.is_some() {
            statements.push(self.parse_statement());
        }
        statements
    }

    fn is_next(&mut self, tokens: &[TokenKind]) -> bool {
        for token in tokens {
            if self.check(*token) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current.unwrap().kind == kind
    }

    fn advance(&mut self) {
        self.previous = self.current;
        self.current = self.tokens.pop_front();
    }

    fn consume(&mut self, kind: TokenKind) -> Option<Token<'source>> {
        if self.check(kind) {
            self.advance();
            return self.previous;
        }
        None
    }

    fn parse_statement(&mut self) -> Statement<'source> {
        if self.is_next(&[TokenKind::Print]) {
            self.parse_print_statement()
        } else {
            unreachable!();
        }
    }

    fn parse_print_statement(&mut self) -> Statement<'source> {
        self.consume(TokenKind::Print);
        let expression = self.parse_expression();
        self.consume(TokenKind::Semicolon);
        Statement::Print(PrintStatement { expression })
    }

    fn parse_expression(&mut self) -> Expression<'source> {
        self.term()
    }

    fn term(&mut self) -> Expression<'source> {
        let mut result = self.primary();
        while self.is_next(&[TokenKind::Plus]) {
            let kind = match self.previous {
                Some(token) => match token.kind {
                    TokenKind::Plus => BinaryExpressionKind::Add,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: Box::new(result),
                rhs: Box::new(self.primary()),
            });
        }
        result
    }

    fn primary(&mut self) -> Expression<'source> {
        if self.is_next(&[TokenKind::Number]) {
            let n = self.previous.unwrap().value.parse().unwrap();
            Expression::Literal(LiteralExpression {
                value: Literal::Num(n),
            })
        } else {
            unreachable!();
        }
    }
}

#[derive(Debug)]
enum Statement<'source> {
    Print(PrintStatement<'source>),
}

#[derive(Debug)]
struct PrintStatement<'source> {
    expression: Expression<'source>,
}

#[derive(Debug)]
enum Expression<'source> {
    Literal(LiteralExpression<'source>),
    Binary(BinaryExpression<'source>),
}

#[derive(Debug)]
struct LiteralExpression<'source> {
    value: Literal<'source>,
}

#[derive(Debug)]
struct BinaryExpression<'source> {
    kind: BinaryExpressionKind,
    lhs: Box<Expression<'source>>,
    rhs: Box<Expression<'source>>,
}

#[derive(Debug)]
enum BinaryExpressionKind {
    Add,
}

#[allow(dead_code)]
#[derive(Debug)]
enum Literal<'source> {
    Num(f64),
    String(&'source str),
}

struct Compiler<'source> {
    bytecode: Vec<Opcode<'source>>,
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'source> Compiler<'source> {
    fn new() -> Self {
        Compiler {
            bytecode: Vec::new(),
        }
    }

    fn compile(&mut self, ast: Vec<Statement<'source>>) -> &[Opcode<'source>] {
        for statement in ast {
            statement.codegen(self);
        }
        self.bytecode.push(Opcode::Halt);
        self.bytecode.as_slice()
    }

    fn emit_bytes(&mut self, opcodes: &[Opcode<'source>]) -> usize {
        for opcode in opcodes {
            self.bytecode.push(*opcode);
        }
        self.bytecode.len() - opcodes.len()
    }
}

impl<'source> Codegen<'source> for Statement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        match self {
            Statement::Print(print_statement) => print_statement.codegen(compiler),
        }
    }
}

impl<'source> Codegen<'source> for PrintStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        self.expression.codegen(compiler);
        compiler.emit_bytes(&[Opcode::Print]);
    }
}

impl<'source> Codegen<'source> for Expression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        match self {
            Expression::Binary(binexp) => binexp.codegen(compiler),
            Expression::Literal(literal) => literal.codegen(compiler),
        }
    }
}

impl<'source> Codegen<'source> for LiteralExpression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        match &self.value {
            Literal::Num(n) => {
                compiler.emit_bytes(&[Opcode::Const(*n)]);
            }
            Literal::String(s) => {
                compiler.emit_bytes(&[Opcode::Str(s)]);
            }
        }
    }
}

impl<'source> Codegen<'source> for BinaryExpression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        self.lhs.codegen(compiler);
        self.rhs.codegen(compiler);

        match self.kind {
            BinaryExpressionKind::Add => {
                compiler.emit_bytes(&[Opcode::Add]);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Opcode<'source> {
    Print,
    Const(f64),
    Add,
    Str(&'source str),
    Halt,
}

trait Codegen<'source> {
    fn codegen(&self, _compiler: &mut Compiler<'source>) {}
}

#[derive(Debug, PartialEq, Clone)]
enum Object {
    Number(f64),
    String(Rc<String>),
}

impl std::ops::Add for Object {
    type Output = Object;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => (a + b).into(),
            _ => unreachable!(),
        }
    }
}

impl From<f64> for Object {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

macro_rules! binop {
    ($self:tt, $op:tt) => {
        {
            let b = $self.stack.pop().unwrap();
            let a = $self.stack.pop().unwrap();
            $self.stack.push((a $op b).into());
        }
    };
}

struct VM<'source, 'bytecode> {
    bytecode: &'bytecode [Opcode<'source>],
    stack: Vec<Object>,
    ip: usize,
}

const STACK_MIN: usize = 1024;

impl<'source, 'bytecode> VM<'source, 'bytecode> {
    fn new(bytecode: &'bytecode [Opcode<'source>]) -> VM<'source, 'bytecode> {
        VM {
            bytecode,
            stack: Vec::with_capacity(STACK_MIN),
            ip: 0,
        }
    }

    fn run(&mut self) {
        loop {
            match unsafe { *self.bytecode.get_unchecked(self.ip) } {
                Opcode::Const(n) => self.handle_op_const(n),
                Opcode::Str(s) => self.handle_op_str(s),
                Opcode::Print => self.handle_op_print(),
                Opcode::Add => self.handle_op_add(),
                Opcode::Halt => break,
            }

            self.ip += 1;
        }
    }

    fn handle_op_const(&mut self, n: f64) {
        self.stack.push(n.into());
    }

    fn handle_op_str(&mut self, s: &str) {
        self.stack.push(s.to_owned().into());
    }

    fn handle_op_print(&mut self) {
        let obj = self.stack.pop();
        if let Some(o) = obj {
            println!("{:?}", o);
        }
    }

    fn handle_op_add(&mut self) {
        binop!(self, +);
    }
}

fn main() {
    let src = String::from("print 1 + 2;");
    let tokenizer = Tokenizer::new(&src);
    let mut parser = Parser::new();
    let mut compiler = Compiler::new();
    let ast = parser.parse(tokenizer.into_iter().collect());
    let bytecode = compiler.compile(ast);
    let mut vm = VM::new(bytecode);
    vm.run();
}
