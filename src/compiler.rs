use crate::parser::{
    AssignExpression, BinaryExpression, BinaryExpressionKind, BlockStatement, CallExpression,
    Expression, ExpressionStatement, FnStatement, IfStatement, Literal, LiteralExpression,
    PrintStatement, ReturnStatement, Statement, VariableExpression, WhileStatement,
};
use std::collections::HashMap;

const CAPACITY_MIN: usize = 1024;

pub struct Compiler<'src> {
    bytecode: Vec<Opcode<'src>>,
    functions: HashMap<&'src str, usize>,
    locals: Vec<&'src str>,
    depth: usize,
    pops: [usize; CAPACITY_MIN],
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'src> Compiler<'src> {
    pub fn new() -> Self {
        Compiler {
            bytecode: Vec::with_capacity(CAPACITY_MIN),
            functions: HashMap::with_capacity(CAPACITY_MIN),
            locals: Vec::with_capacity(CAPACITY_MIN),
            depth: 0,
            pops: [0; CAPACITY_MIN],
        }
    }

    pub fn compile(&mut self, ast: &[Statement<'src>]) -> &[Opcode<'src>] {
        for statement in ast {
            statement.codegen(self);
        }
        self.bytecode.push(Opcode::Halt);
        self.bytecode.as_slice()
    }

    fn emit_opcodes(&mut self, opcodes: &[Opcode<'src>]) -> usize {
        for opcode in opcodes {
            self.bytecode.push(*opcode);
        }
        self.bytecode.len() - opcodes.len()
    }

    fn emit_stack_cleanup(&mut self) {
        let popcount = self.pops[self.depth];
        for _ in 0..popcount {
            self.bytecode.push(Opcode::Pop);
        }
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        self.locals.iter().position(|&local| local == name)
    }

    fn patch_jmp(&mut self, idx: usize) {
        let v = self.bytecode.len() - 1;
        match self.bytecode[idx] {
            Opcode::Jmp(ref mut addr) | Opcode::Jz(ref mut addr) => *addr = v,
            _ => unreachable!(),
        }
    }
}

impl<'src> Codegen<'src> for Statement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        match self {
            Statement::Print(print_statement) => print_statement.codegen(compiler),
            Statement::Fn(fn_statement) => fn_statement.codegen(compiler),
            Statement::Return(return_statement) => return_statement.codegen(compiler),
            Statement::If(if_statement) => if_statement.codegen(compiler),
            Statement::While(while_statement) => while_statement.codegen(compiler),
            Statement::Expression(expr_statement) => expr_statement.codegen(compiler),
            Statement::Block(block_statement) => block_statement.codegen(compiler),
            Statement::Dummy => {}
        }
    }
}

impl<'src> Codegen<'src> for PrintStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        self.expression.codegen(compiler);
        compiler.emit_opcodes(&[Opcode::Print]);
    }
}

impl<'src> Codegen<'src> for FnStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        let jmp_idx = compiler.emit_opcodes(&[Opcode::Jmp(0xFFFF)]);

        compiler.functions.insert(self.name, jmp_idx);

        for argument in &self.arguments {
            compiler.locals.push(argument);
        }

        compiler.pops[1] = compiler.locals.len();

        if let Statement::Block(block) = &*self.body {
            block.codegen(compiler);
        }

        compiler.emit_stack_cleanup();

        compiler.emit_opcodes(&[Opcode::Null, Opcode::Ret]);

        compiler.patch_jmp(jmp_idx);

        compiler.locals.clear();
    }
}

impl<'src> Codegen<'src> for IfStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        self.condition.codegen(compiler);

        let jz_idx = compiler.emit_opcodes(&[Opcode::Jz(0xFFFF)]);
        self.if_branch.codegen(compiler);
        compiler.patch_jmp(jz_idx);

        let else_idx = compiler.emit_opcodes(&[Opcode::Jmp(0xFFFF)]);
        self.else_branch.codegen(compiler);
        compiler.patch_jmp(else_idx);
    }
}

impl<'src> Codegen<'src> for WhileStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        let loop_start = compiler.bytecode.len() - 1;
        self.condition.codegen(compiler);
        let jz_idx = compiler.emit_opcodes(&[Opcode::Jz(0xFFFF)]);
        self.body.codegen(compiler);
        compiler.emit_opcodes(&[Opcode::Jmp(loop_start)]);
        compiler.patch_jmp(jz_idx);
    }
}

impl<'src> Codegen<'src> for ExpressionStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        match &self.expression {
            Expression::Call(call_expr) => {
                call_expr.codegen(compiler);
                compiler.emit_opcodes(&[Opcode::Pop]);
            }
            Expression::Assign(assign_expr) => {
                assign_expr.codegen(compiler);
            }
            _ => unreachable!(),
        }
    }
}

impl<'src> Codegen<'src> for ReturnStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        self.expression.codegen(compiler);
        let mut deepset_no = compiler.locals.len() - 1;
        for _ in 0..compiler.locals.len() {
            compiler.emit_opcodes(&[Opcode::Deepset(deepset_no)]);
            deepset_no = deepset_no.saturating_sub(1);
        }
        compiler.emit_opcodes(&[Opcode::Ret]);
    }
}

impl<'src> Codegen<'src> for BlockStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        compiler.depth += 1;
        for statement in &self.body {
            statement.codegen(compiler);
        }
        compiler.emit_stack_cleanup();
        compiler.depth -= 1;
    }
}

impl<'src> Codegen<'src> for Expression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        match self {
            Expression::Literal(literal) => literal.codegen(compiler),
            Expression::Variable(varexp) => varexp.codegen(compiler),
            Expression::Binary(binexp) => binexp.codegen(compiler),
            Expression::Call(call) => call.codegen(compiler),
            Expression::Assign(assignment) => assignment.codegen(compiler),
        }
    }
}

impl<'src> Codegen<'src> for LiteralExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        match &self.value {
            Literal::Num(n) => {
                compiler.emit_opcodes(&[Opcode::Const(*n)]);
            }
            Literal::Bool(b) => match b {
                true => {
                    compiler.emit_opcodes(&[Opcode::False, Opcode::Not]);
                }
                false => {
                    compiler.emit_opcodes(&[Opcode::False]);
                }
            },
            Literal::String(s) => {
                compiler.emit_opcodes(&[Opcode::Str(s)]);
            }
            Literal::Null => {
                compiler.emit_opcodes(&[Opcode::Null]);
            }
        }
    }
}

impl<'src> Codegen<'src> for VariableExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        let local = compiler.resolve_local(self.value);

        if let Some(idx) = local {
            compiler.emit_opcodes(&[Opcode::Deepget(idx)]);
        } else {
            compiler.locals.push(self.value);
            let idx = compiler.resolve_local(self.value).unwrap();
            compiler.emit_opcodes(&[Opcode::Deepget(idx)]);
        }
    }
}

impl<'src> Codegen<'src> for BinaryExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        self.lhs.codegen(compiler);
        self.rhs.codegen(compiler);

        match self.kind {
            BinaryExpressionKind::Add => {
                compiler.emit_opcodes(&[Opcode::Add]);
            }
            BinaryExpressionKind::Sub => {
                compiler.emit_opcodes(&[Opcode::Sub]);
            }
            BinaryExpressionKind::Mul => {
                compiler.emit_opcodes(&[Opcode::Mul]);
            }
            BinaryExpressionKind::Div => {
                compiler.emit_opcodes(&[Opcode::Div]);
            }
            BinaryExpressionKind::Equality(negation) => {
                compiler.emit_opcodes(&[Opcode::Eq]);
                if negation {
                    compiler.emit_opcodes(&[Opcode::Not]);
                }
            }
            BinaryExpressionKind::Less => {
                compiler.emit_opcodes(&[Opcode::Lt]);
            }
            BinaryExpressionKind::Greater => {
                compiler.emit_opcodes(&[Opcode::Gt]);
            }
            BinaryExpressionKind::LessEqual => {
                compiler.emit_opcodes(&[Opcode::Gt, Opcode::Not]);
            }
            BinaryExpressionKind::GreaterEqual => {
                compiler.emit_opcodes(&[Opcode::Lt, Opcode::Not]);
            }
            BinaryExpressionKind::Strcat => {
                compiler.emit_opcodes(&[Opcode::Strcat]);
            }
        }
    }
}

impl<'src> Codegen<'src> for CallExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        for argument in &self.arguments {
            argument.codegen(compiler);
        }

        let jmp_addr = compiler.functions.get(&self.variable).unwrap();

        compiler.emit_opcodes(&[Opcode::Call(self.arguments.len()), Opcode::Jmp(*jmp_addr)]);
    }
}

impl<'src> Codegen<'src> for AssignExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        let variable_name = match &*self.lhs {
            Expression::Variable(variable) => &variable.value,
            _ => unimplemented!(),
        };
        self.rhs.codegen(compiler);

        let local = compiler.resolve_local(variable_name);

        if let Some(idx) = local {
            compiler.emit_opcodes(&[Opcode::Deepset(idx)]);
        } else {
            compiler.locals.push(variable_name);
            compiler.pops[compiler.depth] += 1;
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode<'src> {
    Print,
    Const(f64),
    Add,
    Sub,
    Mul,
    Div,
    False,
    Not,
    Null,
    Eq,
    Lt,
    Gt,
    Str(&'src str),
    Jmp(usize),
    Jz(usize),
    Call(usize),
    Ret,
    Deepget(usize),
    Deepset(usize),
    Strcat,
    Pop,
    Halt,
}

trait Codegen<'src> {
    fn codegen(&self, _compiler: &mut Compiler<'src>) {}
}
