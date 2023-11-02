use crate::bail_out;
use crate::parser::{
    AssignExpression, BinaryExpression, BinaryExpressionKind, BlockStatement, CallExpression,
    Expression, ExpressionStatement, FnStatement, GetExpression, IfStatement, Literal,
    LiteralExpression, PrintStatement, ReturnStatement, Statement, StructExpression,
    StructInitializerExpression, StructStatement, UnaryExpression, VariableExpression,
    WhileStatement,
};
use crate::tokenizer::Token;
use std::collections::HashMap;

const CAPACITY_MIN: usize = 1024;

pub struct Compiler<'src> {
    bytecode: Vec<Opcode<'src>>,
    functions: HashMap<&'src str, usize>,
    locals: Vec<&'src str>,
    structs: HashMap<&'src str, Vec<&'src str>>,
    pops: Vec<usize>,
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
            structs: HashMap::with_capacity(CAPACITY_MIN),
            pops: Vec::with_capacity(CAPACITY_MIN),
        }
    }

    pub fn compile(&mut self, ast: &[Statement<'src>]) -> &[Opcode<'src>] {
        for statement in ast {
            statement.codegen(self);
        }

        match self.functions.get("main") {
            Some(&addr) => {
                self.emit_opcodes(&[Opcode::Call(0), Opcode::Jmp(addr), Opcode::Pop]);
            }
            None => bail_out!(compiler, "The main fn was not defined."),
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
        let popcount = self.pops.last().unwrap();
        for _ in 0..*popcount {
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
            Statement::Struct(struct_statement) => struct_statement.codegen(compiler),
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

        if let Token::Identifier(name) = self.name {
            compiler.functions.insert(name, jmp_idx);
        } else {
            unreachable!();
        }

        for argument in &self.arguments {
            if let Token::Identifier(ident) = argument {
                compiler.locals.push(ident);
            } else {
                unreachable!();
            }
        }

        compiler.pops.push(compiler.locals.len());

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

impl<'src> Codegen<'src> for StructStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        compiler.structs.insert(self.name, self.members.clone());
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
        compiler.pops.push(0);
        for statement in &self.body {
            statement.codegen(compiler);
        }
        compiler.emit_stack_cleanup();
        compiler.pops.pop();
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
            Expression::Unary(unary) => unary.codegen(compiler),
            Expression::Get(getexp) => getexp.codegen(compiler),
            Expression::Struct(structexp) => structexp.codegen(compiler),
            Expression::StructInitializer(structinitexp) => structinitexp.codegen(compiler),
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
            if let Some(last) = compiler.pops.last_mut() {
                *last += 1;
            }
        }
    }
}

impl<'src> Codegen<'src> for UnaryExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        match self.op {
            Token::Minus => {
                self.expr.codegen(compiler);
                compiler.emit_opcodes(&[Opcode::Neg]);
            }
            Token::Bang => {
                self.expr.codegen(compiler);
                compiler.emit_opcodes(&[Opcode::Not]);
            }
            _ => unreachable!(),
        }
    }
}

impl<'src> Codegen<'src> for GetExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        self.expr.codegen(compiler);
        compiler.emit_opcodes(&[Opcode::Getattr(self.member)]);
    }
}

impl<'src> Codegen<'src> for StructExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        if let Some(s) = compiler.structs.get(self.name) {
            if s.len() != self.initializers.len() {
                bail_out!(compiler, "struct '{}' has {} members.", self.name, s.len());
            }

            compiler.emit_opcodes(&[Opcode::Struct(self.name)]);

            for init in &self.initializers {
                init.codegen(compiler);
            }
        } else {
            bail_out!(compiler, "struct '{}' is not defined", self.name);
        }
    }
}

impl<'src> Codegen<'src> for StructInitializerExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) {
        self.value.codegen(compiler);
        if let Expression::Variable(var) = &*self.member {
            compiler.emit_opcodes(&[Opcode::Setattr(var.value)]);
        } else {
            unreachable!();
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
    Neg,
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
    Getattr(&'src str),
    Setattr(&'src str),
    Strcat,
    Struct(&'src str),
    Pop,
    Halt,
}

trait Codegen<'src> {
    fn codegen(&self, _compiler: &mut Compiler<'src>) {}
}
