use crate::parser::{
    BinaryExpression, BinaryExpressionKind, Expression, Literal, LiteralExpression, PrintStatement,
    Statement,
};

pub struct Compiler<'source> {
    bytecode: Vec<Opcode<'source>>,
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'source> Compiler<'source> {
    pub fn new() -> Self {
        Compiler {
            bytecode: Vec::new(),
        }
    }

    pub fn compile(&mut self, ast: Vec<Statement<'source>>) -> &[Opcode<'source>] {
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
            BinaryExpressionKind::Sub => {
                compiler.emit_bytes(&[Opcode::Sub]);
            }
            BinaryExpressionKind::Mul => {
                compiler.emit_bytes(&[Opcode::Mul]);
            }
            BinaryExpressionKind::Div => {
                compiler.emit_bytes(&[Opcode::Div]);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode<'source> {
    Print,
    Const(f64),
    Add,
    Sub,
    Mul,
    Div,
    Str(&'source str),
    Halt,
}

trait Codegen<'source> {
    fn codegen(&self, _compiler: &mut Compiler<'source>) {}
}
