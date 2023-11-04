use crate::parser::{
    AssignExpression, BinaryExpression, BinaryExpressionKind, BlockStatement, CallExpression,
    Expression, ExpressionStatement, FnStatement, GetExpression, IfStatement, Literal,
    LiteralExpression, PrintStatement, ReturnStatement, Statement, StructExpression,
    StructInitializerExpression, StructStatement, UnaryExpression, VariableExpression,
    WhileStatement,
};
use crate::tokenizer::Token;
use anyhow::{bail, Result};
use std::collections::HashMap;
use std::rc::Rc;

const CAPACITY_MIN: usize = 1024;

pub struct Compiler<'src> {
    bytecode: Vec<Opcode>,
    functions: HashMap<&'src str, Function<'src>>,
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

    pub fn compile(&mut self, ast: &[Statement<'src>]) -> Result<&[Opcode]> {
        for statement in ast {
            statement.codegen(self)?;
        }

        match self.functions.get("main") {
            Some(f) => {
                self.emit_opcodes(&[Opcode::Call(0), Opcode::Jmp(f.location), Opcode::Pop(1)]);
            }
            None => bail!("compiler: main fn was not defined"),
        }

        self.bytecode.push(Opcode::Halt);
        Ok(self.bytecode.as_slice())
    }

    fn emit_opcodes(&mut self, opcodes: &[Opcode]) -> usize {
        for opcode in opcodes {
            self.bytecode.push(opcode.clone());
        }
        self.bytecode.len() - opcodes.len()
    }

    fn emit_stack_cleanup(&mut self) {
        let popcount = self.pops.last().unwrap();
        self.bytecode.push(Opcode::Pop(*popcount));
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
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        match self {
            Statement::Print(print_statement) => print_statement.codegen(compiler)?,
            Statement::Fn(fn_statement) => fn_statement.codegen(compiler)?,
            Statement::Return(return_statement) => return_statement.codegen(compiler)?,
            Statement::If(if_statement) => if_statement.codegen(compiler)?,
            Statement::While(while_statement) => while_statement.codegen(compiler)?,
            Statement::Expression(expr_statement) => expr_statement.codegen(compiler)?,
            Statement::Block(block_statement) => block_statement.codegen(compiler)?,
            Statement::Struct(struct_statement) => struct_statement.codegen(compiler)?,
            Statement::Dummy => {}
        }

        Ok(())
    }
}

impl<'src> Codegen<'src> for PrintStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        self.expression.codegen(compiler)?;
        compiler.emit_opcodes(&[Opcode::Print]);

        Ok(())
    }
}

impl<'src> Codegen<'src> for FnStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        let jmp_idx = compiler.emit_opcodes(&[Opcode::Jmp(0xFFFF)]);

        let arguments: Vec<&'src str> = self
            .arguments
            .iter()
            .map(|&token| token.get_value())
            .collect();

        let name = self.name.get_value();

        let f = Function {
            name,
            localscount: 0,
            location: jmp_idx,
            paramcount: arguments.len(),
        };
        compiler.functions.insert(name, f.clone());

        for argument in &self.arguments {
            compiler.locals.push(argument.get_value());
        }

        compiler.pops.push(compiler.locals.len());

        if let Statement::Block(block) = &*self.body {
            block.codegen(compiler)?;
        }

        compiler.patch_jmp(jmp_idx);

        let f = compiler.functions.get_mut(f.name).unwrap();
        f.localscount = compiler.locals.len();

        compiler.locals.clear();
        compiler.pops.clear();

        Ok(())
    }
}

impl<'src> Codegen<'src> for IfStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        self.condition.codegen(compiler)?;

        let jz_idx = compiler.emit_opcodes(&[Opcode::Jz(0xFFFF)]);
        self.if_branch.codegen(compiler)?;
        compiler.patch_jmp(jz_idx);

        let else_idx = compiler.emit_opcodes(&[Opcode::Jmp(0xFFFF)]);
        self.else_branch.codegen(compiler)?;
        compiler.patch_jmp(else_idx);

        Ok(())
    }
}

impl<'src> Codegen<'src> for WhileStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        let loop_start = compiler.bytecode.len() - 1;
        self.condition.codegen(compiler)?;
        let jz_idx = compiler.emit_opcodes(&[Opcode::Jz(0xFFFF)]);
        self.body.codegen(compiler)?;
        compiler.emit_opcodes(&[Opcode::Jmp(loop_start)]);
        compiler.patch_jmp(jz_idx);

        Ok(())
    }
}

impl<'src> Codegen<'src> for StructStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        compiler.structs.insert(self.name, self.members.clone());
        Ok(())
    }
}

impl<'src> Codegen<'src> for ExpressionStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        match &self.expression {
            Expression::Call(call_expr) => {
                call_expr.codegen(compiler)?;
                compiler.emit_opcodes(&[Opcode::Pop(1)]);
            }
            Expression::Assign(assign_expr) => {
                assign_expr.codegen(compiler)?;
            }
            _ => unreachable!(),
        }

        Ok(())
    }
}

impl<'src> Codegen<'src> for ReturnStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        self.expression.codegen(compiler)?;
        let mut deepset_no = compiler.locals.len().saturating_sub(1);
        for _ in 0..compiler.locals.len() {
            compiler.emit_opcodes(&[Opcode::Deepset(deepset_no)]);
            deepset_no = deepset_no.saturating_sub(1);
        }
        compiler.emit_opcodes(&[Opcode::Ret]);
        compiler.emit_stack_cleanup();

        Ok(())
    }
}

impl<'src> Codegen<'src> for BlockStatement<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        compiler.pops.push(0);
        for statement in &self.body {
            statement.codegen(compiler)?;
        }
        for _ in 0..*compiler.pops.last().unwrap() {
            compiler.locals.pop();
        }
        compiler.emit_stack_cleanup();
        compiler.pops.pop();

        Ok(())
    }
}

impl<'src> Codegen<'src> for Expression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        match self {
            Expression::Literal(literal) => literal.codegen(compiler)?,
            Expression::Variable(varexp) => varexp.codegen(compiler)?,
            Expression::Binary(binexp) => binexp.codegen(compiler)?,
            Expression::Call(call) => call.codegen(compiler)?,
            Expression::Assign(assignment) => assignment.codegen(compiler)?,
            Expression::Unary(unary) => unary.codegen(compiler)?,
            Expression::Get(getexp) => getexp.codegen(compiler)?,
            Expression::Struct(structexp) => structexp.codegen(compiler)?,
            Expression::StructInitializer(structinitexp) => structinitexp.codegen(compiler)?,
        }

        Ok(())
    }
}

impl<'src> Codegen<'src> for LiteralExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
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
                compiler.emit_opcodes(&[Opcode::Str(s.to_string().into())]);
            }
            Literal::Null => {
                compiler.emit_opcodes(&[Opcode::Null]);
            }
        }

        Ok(())
    }
}

impl<'src> Codegen<'src> for VariableExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        let local = compiler.resolve_local(self.value);

        if let Some(idx) = local {
            compiler.emit_opcodes(&[Opcode::Deepget(idx)]);
        } else {
            compiler.locals.push(self.value);
            let idx = compiler.resolve_local(self.value).unwrap();
            compiler.emit_opcodes(&[Opcode::Deepget(idx)]);
        }

        Ok(())
    }
}

impl<'src> Codegen<'src> for BinaryExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        self.lhs.codegen(compiler)?;
        self.rhs.codegen(compiler)?;

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

        Ok(())
    }
}

impl<'src> Codegen<'src> for CallExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        let f = compiler.functions.get(&self.variable);

        if f.is_none() {
            bail!("compiler: function '{}' is not defined", self.variable);
        }

        let f = f.unwrap();

        if f.paramcount != self.arguments.len() {
            bail!(
                "compiler: function '{}' takes {} arguments",
                f.name,
                f.paramcount
            );
        }

        let addr = f.location;

        for argument in &self.arguments {
            argument.codegen(compiler)?;
        }

        compiler.emit_opcodes(&[Opcode::Call(self.arguments.len()), Opcode::Jmp(addr)]);

        Ok(())
    }
}

impl<'src> Codegen<'src> for AssignExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        match &*self.lhs {
            Expression::Variable(variable) => {
                self.rhs.codegen(compiler)?;

                let local = compiler.resolve_local(variable.value);

                if let Some(idx) = local {
                    compiler.emit_opcodes(&[Opcode::Deepset(idx)]);
                } else {
                    compiler.locals.push(variable.value);
                    if let Some(last) = compiler.pops.last_mut() {
                        *last += 1;
                    }
                }
            }
            Expression::Unary(_) => {
                self.rhs.codegen(compiler)?;

                let mut current = self.lhs.clone();
                while match *current.clone() {
                    Expression::Unary(u) => {
                        current = u.expr;
                        true
                    }
                    _ => false,
                } {}

                if let Expression::Variable(var) = *current.clone() {
                    let local = compiler.resolve_local(var.value);
                    if let Some(idx) = local {
                        compiler.emit_opcodes(&[Opcode::DeepsetDeref(idx)]);
                    } else {
                        compiler.locals.push(var.value);
                        let idx = compiler.resolve_local(var.value).unwrap();
                        compiler.emit_opcodes(&[Opcode::DeepsetDeref(idx)]);
                    }
                } else {
                    bail!("compiler: tried to deref a non-var");
                }
            }
            _ => unimplemented!(),
        };

        Ok(())
    }
}

impl<'src> Codegen<'src> for UnaryExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        match self.op {
            Token::Minus => {
                self.expr.codegen(compiler)?;
                compiler.emit_opcodes(&[Opcode::Neg]);
            }
            Token::Bang => {
                self.expr.codegen(compiler)?;
                compiler.emit_opcodes(&[Opcode::Not]);
            }
            Token::Ampersand => match &*self.expr {
                Expression::Variable(var) => {
                    let local = compiler.resolve_local(var.value);
                    if let Some(idx) = local {
                        compiler.emit_opcodes(&[Opcode::DeepgetPtr(idx)]);
                    } else {
                        compiler.locals.push(var.value);
                        let idx = compiler.resolve_local(var.value).unwrap();
                        compiler.emit_opcodes(&[Opcode::DeepgetPtr(idx)]);
                    }
                }
                _ => bail!("compiler: expected variable"),
            },
            Token::Star => {
                self.expr.codegen(compiler)?;
                compiler.emit_opcodes(&[Opcode::Deref]);
            }
            _ => unreachable!(),
        }

        Ok(())
    }
}

impl<'src> Codegen<'src> for GetExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        self.expr.codegen(compiler)?;
        compiler.emit_opcodes(&[Opcode::Getattr(self.member.to_string().into())]);

        Ok(())
    }
}

impl<'src> Codegen<'src> for StructExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        if let Some(s) = compiler.structs.get(self.name) {
            if s.len() != self.initializers.len() {
                bail!("compiler: struct '{}' has {} members", self.name, s.len());
            }

            compiler.emit_opcodes(&[Opcode::Struct(self.name.to_string().into())]);

            for init in &self.initializers {
                init.codegen(compiler)?;
            }
        } else {
            bail!("compiler: struct '{}' is not defined", self.name);
        }

        Ok(())
    }
}

impl<'src> Codegen<'src> for StructInitializerExpression<'src> {
    fn codegen(&self, compiler: &mut Compiler<'src>) -> Result<()> {
        self.value.codegen(compiler)?;
        if let Expression::Variable(var) = &*self.member {
            compiler.emit_opcodes(&[Opcode::Setattr(var.value.to_string().into())]);
        } else {
            unreachable!();
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Opcode {
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
    Str(Rc<String>),
    Jmp(usize),
    Jz(usize),
    Call(usize),
    Ret,
    Deepget(usize),
    DeepgetPtr(usize),
    Deepset(usize),
    DeepsetDeref(usize),
    Deref,
    Getattr(Rc<String>),
    Setattr(Rc<String>),
    Strcat,
    Struct(Rc<String>),
    Pop(usize),
    Halt,
}

trait Codegen<'src> {
    fn codegen(&self, _compiler: &mut Compiler<'src>) -> Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Function<'src> {
    name: &'src str,
    location: usize,
    paramcount: usize,
    localscount: usize,
}
