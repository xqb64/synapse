use crate::compiler::Opcode;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Number(f64),
    Bool(bool),
    String(Rc<str>),
    Null,
}

#[derive(Debug, Clone, Copy)]
enum InternalObject {
    BytecodePtr(usize, usize),
}

macro_rules! runtime_error {
    ($msg:expr) => {{
        eprintln!("{}", $msg);
        std::process::exit(1);
    }};
}

impl std::ops::Add for Object {
    type Output = Object;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => (a + b).into(),
            _ => runtime_error!("You can only + numbers."),
        }
    }
}

impl std::ops::Sub for Object {
    type Output = Object;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => (a - b).into(),
            _ => runtime_error!("You can only - numbers."),
        }
    }
}

impl std::ops::Mul for Object {
    type Output = Object;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => (a * b).into(),
            _ => runtime_error!("You can only * numbers."),
        }
    }
}

impl std::ops::Div for Object {
    type Output = Object;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => (a / b).into(),
            _ => runtime_error!("You can only / numbers."),
        }
    }
}

impl std::ops::Not for Object {
    type Output = Object;

    fn not(self) -> Self::Output {
        match self {
            Object::Bool(b) => (!b).into(),
            _ => runtime_error!("You can only ! booleans."),
        }
    }
}

impl std::cmp::PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => a.partial_cmp(b),
            _ => runtime_error!("You can only <, >, <=, >= numbers."),
        }
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Self::Bool(value)
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

macro_rules! adjust_idx {
    ($self:tt, $index:expr) => {{
        let (fp, idx) = match $self.frame_ptrs.last() {
            Some(&internal_obj) => {
                let InternalObject::BytecodePtr(_, location) = internal_obj;
                (location, $index)
            }
            None => (0, $index),
        };
        fp + idx
    }};
}

pub struct VM<'source, 'bytecode> {
    bytecode: &'bytecode [Opcode<'source>],
    stack: Vec<Object>,
    frame_ptrs: Vec<InternalObject>,
    ip: usize,
}

const STACK_MIN: usize = 1024;

impl<'source, 'bytecode> VM<'source, 'bytecode> {
    pub fn new(bytecode: &'bytecode [Opcode<'source>]) -> VM<'source, 'bytecode> {
        VM {
            bytecode,
            stack: Vec::with_capacity(STACK_MIN),
            frame_ptrs: Vec::with_capacity(STACK_MIN),
            ip: 0,
        }
    }

    pub fn run(&mut self) {
        loop {
            if cfg!(debug_assertions) {
                println!("current instruction: {:?}", self.bytecode[self.ip]);
            }

            match unsafe { *self.bytecode.get_unchecked(self.ip) } {
                Opcode::Const(n) => self.handle_op_const(n),
                Opcode::Str(s) => self.handle_op_str(s),
                Opcode::Print => self.handle_op_print(),
                Opcode::Add => self.handle_op_add(),
                Opcode::Sub => self.handle_op_sub(),
                Opcode::Mul => self.handle_op_mul(),
                Opcode::Div => self.handle_op_div(),
                Opcode::False => self.handle_op_false(),
                Opcode::Not => self.handle_op_not(),
                Opcode::Null => self.handle_op_null(),
                Opcode::Eq => self.handle_op_eq(),
                Opcode::Lt => self.handle_op_lt(),
                Opcode::Gt => self.handle_op_gt(),
                Opcode::Jmp(addr) => self.handle_op_jmp(addr),
                Opcode::Jz(addr) => self.handle_op_jz(addr),
                Opcode::Call(n) => self.handle_op_call(n),
                Opcode::Ret => self.handle_op_ret(),
                Opcode::Deepget(idx) => self.handle_op_deepget(idx),
                Opcode::Deepset(idx) => self.handle_op_deepset(idx),
                Opcode::Strcat => self.handle_op_strcat(),
                Opcode::Pop => self.handle_op_pop(),
                Opcode::Halt => break,
            }

            if cfg!(debug_assertions) {
                println!("stack: {:?}", self.stack);
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

    fn handle_op_strcat(&mut self) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        match (a, b) {
            (Object::String(a), Object::String(b)) => {
                self.stack.push(format!("{}{}", a, b).into());
            }
            _ => {
                runtime_error!("Can only concatenate two strings.");
            }
        }
    }

    fn handle_op_print(&mut self) {
        let obj = self.stack.pop();
        if let Some(o) = obj {
            if cfg!(debug_assertions) {
                print!("dbg: ");
            }
            println!("{:?}", o);
        }
    }

    fn handle_op_add(&mut self) {
        binop!(self, +);
    }

    fn handle_op_sub(&mut self) {
        binop!(self, -);
    }

    fn handle_op_mul(&mut self) {
        binop!(self, *);
    }

    fn handle_op_div(&mut self) {
        binop!(self, /);
    }

    fn handle_op_false(&mut self) {
        self.stack.push(false.into());
    }

    fn handle_op_not(&mut self) {
        let obj = self.stack.pop().unwrap();
        self.stack.push(!obj);
    }

    fn handle_op_null(&mut self) {
        self.stack.push(Object::Null);
    }

    fn handle_op_eq(&mut self) {
        binop!(self, ==);
    }

    fn handle_op_lt(&mut self) {
        binop!(self, <);
    }

    fn handle_op_gt(&mut self) {
        binop!(self, >);
    }

    fn handle_op_jmp(&mut self, addr: usize) {
        self.ip = addr;
    }

    fn handle_op_jz(&mut self, addr: usize) {
        let item = self.stack.pop().unwrap();
        if let Object::Bool(_b @ false) = item {
            self.ip = addr;
        }
    }

    fn handle_op_call(&mut self, n: usize) {
        self.frame_ptrs.push(InternalObject::BytecodePtr(
            self.ip + 1,
            self.stack.len() - n,
        ));
    }

    fn handle_op_ret(&mut self) {
        let retaddr = self.frame_ptrs.pop().unwrap();
        let InternalObject::BytecodePtr(ptr, _) = retaddr;
        self.ip = ptr;
    }

    fn handle_op_deepget(&mut self, idx: usize) {
        let item = unsafe { self.stack.get_unchecked(adjust_idx!(self, idx)) }.clone();
        self.stack.push(item);
    }

    fn handle_op_deepset(&mut self, idx: usize) {
        self.stack.swap_remove(adjust_idx!(self, idx));
    }

    fn handle_op_pop(&mut self) {
        self.stack.pop();
    }
}
