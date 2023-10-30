use crate::parser::{
    AssignExpression, BinaryExpression, BinaryExpressionKind, BlockStatement, CallExpression,
    Expression, ExpressionStatement, FnStatement, IfStatement, Literal, LiteralExpression,
    PrintStatement, ReturnStatement, Statement, VariableExpression, WhileStatement,
};

pub struct Compiler<'source> {
    bytecode: Vec<Opcode<'source>>,
    functions: std::collections::HashMap<&'source str, usize>,
    locals: Vec<&'source str>,
    depth: usize,
    pops: [usize; 1024],
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
            functions: std::collections::HashMap::new(),
            locals: Vec::new(),
            depth: 0,
            pops: [0; 1024],
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

    fn emit_stack_cleanup(&mut self) {
        let popcount = self.pops[self.depth];
        for _ in 0..popcount {
            self.bytecode.push(Opcode::Pop);
        }
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        self.locals.iter().position(|local| *local == name)
    }
}

impl<'source> Codegen<'source> for Statement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
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

impl<'source> Codegen<'source> for PrintStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        self.expression.codegen(compiler);
        compiler.emit_bytes(&[Opcode::Print]);
    }
}

impl<'source> Codegen<'source> for FnStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        let jmp_idx = compiler.emit_bytes(&[Opcode::Jmp(0xFFFF)]);

        compiler.functions.insert(self.name, jmp_idx);

        for argument in &self.arguments {
            compiler.locals.push(argument);
        }

        compiler.pops[1] = compiler.locals.len();

        if let Statement::Block(block) = &*self.body {
            block.codegen(compiler);
        }

        compiler.emit_stack_cleanup();

        compiler.emit_bytes(&[Opcode::Null, Opcode::Ret]);

        compiler.bytecode[jmp_idx] = Opcode::Jmp(compiler.bytecode.len() - 1);

        compiler.locals.clear();
    }
}

impl<'source> Codegen<'source> for IfStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        self.condition.codegen(compiler);

        let jz_idx = compiler.emit_bytes(&[Opcode::Jz(0xFFFF)]);
        self.if_branch.codegen(compiler);
        compiler.bytecode[jz_idx] = Opcode::Jz(compiler.bytecode.len() - 1);

        let else_idx = compiler.emit_bytes(&[Opcode::Jmp(0xFFFF)]);
        self.else_branch.codegen(compiler);
        compiler.bytecode[else_idx] = Opcode::Jmp(compiler.bytecode.len() - 1);
    }
}

impl<'source> Codegen<'source> for WhileStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        let loop_start = compiler.bytecode.len() - 1;
        self.condition.codegen(compiler);
        let jz_idx = compiler.emit_bytes(&[Opcode::Jz(0xFFFF)]);
        self.body.codegen(compiler);
        compiler.emit_bytes(&[Opcode::Jmp(loop_start)]);
        compiler.bytecode[jz_idx] = Opcode::Jz(compiler.bytecode.len() - 1);
    }
}

impl<'source> Codegen<'source> for ExpressionStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        match &self.expression {
            Expression::Call(call_expr) => {
                call_expr.codegen(compiler);
                compiler.emit_bytes(&[Opcode::Pop]);
            }
            Expression::Assign(assign_expr) => {
                assign_expr.codegen(compiler);
            }
            _ => unreachable!(),
        }
    }
}

impl<'source> Codegen<'source> for ReturnStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        self.expression.codegen(compiler);
        let mut deepset_no = compiler.locals.len() - 1;
        for _ in 0..compiler.locals.len() {
            compiler.emit_bytes(&[Opcode::Deepset(deepset_no)]);
            deepset_no = deepset_no.saturating_sub(1);
        }
        compiler.emit_bytes(&[Opcode::Ret]);
    }
}

impl<'source> Codegen<'source> for BlockStatement<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        compiler.depth += 1;
        for statement in &self.body {
            statement.codegen(compiler);
        }
        compiler.emit_stack_cleanup();
        compiler.depth -= 1;
    }
}

impl<'source> Codegen<'source> for Expression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        match self {
            Expression::Literal(literal) => literal.codegen(compiler),
            Expression::Variable(varexp) => varexp.codegen(compiler),
            Expression::Binary(binexp) => binexp.codegen(compiler),
            Expression::Call(call) => call.codegen(compiler),
            Expression::Assign(assignment) => assignment.codegen(compiler),
        }
    }
}

impl<'source> Codegen<'source> for LiteralExpression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        match &self.value {
            Literal::Num(n) => {
                compiler.emit_bytes(&[Opcode::Const(*n)]);
            }
            Literal::Bool(b) => match b {
                true => {
                    compiler.emit_bytes(&[Opcode::False, Opcode::Not]);
                }
                false => {
                    compiler.emit_bytes(&[Opcode::False]);
                }
            },
            Literal::String(s) => {
                compiler.emit_bytes(&[Opcode::Str(s)]);
            }
            Literal::Null => {
                compiler.emit_bytes(&[Opcode::Null]);
            }
        }
    }
}

impl<'source> Codegen<'source> for VariableExpression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        let local = compiler.resolve_local(self.value);

        if let Some(idx) = local {
            compiler.emit_bytes(&[Opcode::Deepget(idx)]);
        } else {
            compiler.locals.push(self.value);
            let idx = compiler.resolve_local(self.value).unwrap();
            compiler.emit_bytes(&[Opcode::Deepget(idx)]);
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
            BinaryExpressionKind::Equality(negation) => {
                compiler.emit_bytes(&[Opcode::Eq]);
                if negation {
                    compiler.emit_bytes(&[Opcode::Not]);
                }
            }
            BinaryExpressionKind::Less => {
                compiler.emit_bytes(&[Opcode::Lt]);
            }
            BinaryExpressionKind::Greater => {
                compiler.emit_bytes(&[Opcode::Gt]);
            }
            BinaryExpressionKind::LessEqual => {
                compiler.emit_bytes(&[Opcode::Gt, Opcode::Not]);
            }
            BinaryExpressionKind::GreaterEqual => {
                compiler.emit_bytes(&[Opcode::Lt, Opcode::Not]);
            }
        }
    }
}

impl<'source> Codegen<'source> for CallExpression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        for argument in &self.arguments {
            argument.codegen(compiler);
        }

        let jmp_addr = compiler.functions.get(&self.variable).unwrap();

        compiler.emit_bytes(&[Opcode::Call(self.arguments.len()), Opcode::Jmp(*jmp_addr)]);
    }
}

impl<'source> Codegen<'source> for AssignExpression<'source> {
    fn codegen(&self, compiler: &mut Compiler<'source>) {
        let variable_name = match &*self.lhs {
            Expression::Variable(variable) => &variable.value,
            _ => unimplemented!(),
        };
        self.rhs.codegen(compiler);

        let local = compiler.resolve_local(variable_name);

        if let Some(idx) = local {
            compiler.emit_bytes(&[Opcode::Deepset(idx)]);
        } else {
            compiler.locals.push(variable_name);
            compiler.pops[compiler.depth] += 1;
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
    False,
    Not,
    Null,
    Eq,
    Lt,
    Gt,
    Str(&'source str),
    Jmp(usize),
    Jz(usize),
    Call(usize),
    Ret,
    Deepget(usize),
    Deepset(usize),
    Pop,
    Halt,
}

trait Codegen<'source> {
    fn codegen(&self, _compiler: &mut Compiler<'source>) {}
}
