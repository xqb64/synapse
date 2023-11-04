use crate::compiler::Opcode;
use anyhow::{bail, Result};
use std::borrow::Cow;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

macro_rules! pop {
    ($stack:expr) => {{
        $stack.pop()
    }};
}

macro_rules! binop_arithmetic {
    ($self:tt, $op:tt) => {
        {
            let b = pop!($self.stack);
            let a = pop!($self.stack);
            let res = (a $op b);
            match res {
                Ok(r) => $self.stack.push(r.into()),
                Err(e) => bail!(e),
            }
        }
    };
}

macro_rules! binop_relational {
    ($self:tt, $op:tt) => {
        {
            let b = pop!($self.stack);
            let a = pop!($self.stack);
            if std::mem::discriminant(&a) != std::mem::discriminant(&b) {
                bail!("vm: only numbers can be: <, >, <=, >=");
            }
            $self.stack.push((a $op b).into());
        }
    };
}

macro_rules! adjust_idx {
    ($self:tt, $index:expr) => {{
        match $self.frame_ptrs.last() {
            Some(&internal_obj) => {
                let InternalObject::BytecodePtr(_, location) = internal_obj;
                location + $index
            }
            None => 0 + $index,
        }
    }};
}

pub struct VM<'src, 'bytecode> {
    bytecode: &'bytecode [Opcode],
    stack: Stack<'src>,
    frame_ptrs: Vec<InternalObject>,
    ip: usize,
}

const STACK_MIN: usize = 1024;

impl<'src, 'bytecode> VM<'src, 'bytecode>
where
    'bytecode: 'src,
{
    pub fn new(bytecode: &'bytecode [Opcode]) -> VM<'src, 'bytecode> {
        VM {
            bytecode,
            stack: Stack::with_capacity(STACK_MIN),
            frame_ptrs: Vec::with_capacity(STACK_MIN),
            ip: 0,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            if cfg!(debug_assertions) {
                println!("current instruction: {:?}", self.bytecode[self.ip]);
            }

            match unsafe { self.bytecode.get_unchecked(self.ip) } {
                Opcode::Const(n) => self.handle_op_const(*n),
                Opcode::Str(s) => self.handle_op_str(s),
                Opcode::Print => self.handle_op_print(),
                Opcode::Add => self.handle_op_add()?,
                Opcode::Sub => self.handle_op_sub()?,
                Opcode::Mul => self.handle_op_mul()?,
                Opcode::Div => self.handle_op_div()?,
                Opcode::False => self.handle_op_false(),
                Opcode::Not => self.handle_op_not()?,
                Opcode::Neg => self.handle_op_neg()?,
                Opcode::Null => self.handle_op_null(),
                Opcode::Eq => self.handle_op_eq(),
                Opcode::Lt => self.handle_op_lt()?,
                Opcode::Gt => self.handle_op_gt()?,
                Opcode::Jmp(addr) => self.handle_op_jmp(*addr),
                Opcode::Jz(addr) => self.handle_op_jz(*addr),
                Opcode::Call(n) => self.handle_op_call(*n),
                Opcode::Ret => self.handle_op_ret(),
                Opcode::Deepget(idx) => self.handle_op_deepget(*idx),
                Opcode::DeepgetPtr(idx) => self.handle_op_deepgetptr(*idx),
                Opcode::Deepset(idx) => self.handle_op_deepset(*idx),
                Opcode::DeepsetDeref(idx) => self.handle_op_deepsetderef(*idx)?,
                Opcode::Deref => self.handle_op_deref()?,
                Opcode::Getattr(member) => self.handle_op_getattr(member)?,
                Opcode::Setattr(member) => self.handle_op_setattr(member),
                Opcode::Struct(name) => self.handle_op_struct(name),
                Opcode::Strcat => self.handle_op_strcat()?,
                Opcode::Pop(popcount) => self.handle_op_pop(*popcount),
                Opcode::Halt => break Ok(()),
            }

            if cfg!(debug_assertions) {
                println!("stack: {:?}", &self.stack.data[0..self.stack.tos]);
            }

            self.ip += 1;
        }
    }

    fn handle_op_const(&mut self, n: f64) {
        self.stack.push(n.into());
    }

    fn handle_op_str(&mut self, s: &'src str) {
        self.stack.push(s.into());
    }

    fn handle_op_strcat(&mut self) -> Result<()> {
        let b = pop!(self.stack);
        let a = pop!(self.stack);

        match (a, b) {
            (Object::String(a), Object::String(b)) => {
                self.stack
                    .push(format!("{}{}", a.to_owned(), b.to_owned()).into());
            }
            _ => {
                bail!("vm: only strings can be concatenated");
            }
        }

        Ok(())
    }

    fn handle_op_print(&mut self) {
        let obj = pop!(self.stack);
        if cfg!(debug_assertions) {
            print!("dbg: ");
        }
        println!("{:?}", obj);
    }

    fn handle_op_add(&mut self) -> Result<()> {
        binop_arithmetic!(self, +);

        Ok(())
    }

    fn handle_op_sub(&mut self) -> Result<()> {
        binop_arithmetic!(self, -);

        Ok(())
    }

    fn handle_op_mul(&mut self) -> Result<()> {
        binop_arithmetic!(self, *);

        Ok(())
    }

    fn handle_op_div(&mut self) -> Result<()> {
        binop_arithmetic!(self, /);

        Ok(())
    }

    fn handle_op_false(&mut self) {
        self.stack.push(false.into());
    }

    fn handle_op_not(&mut self) -> Result<()> {
        let obj = pop!(self.stack);
        self.stack.push((!obj)?);

        Ok(())
    }

    fn handle_op_neg(&mut self) -> Result<()> {
        let obj = pop!(self.stack);
        self.stack.push((-obj)?);

        Ok(())
    }

    fn handle_op_null(&mut self) {
        self.stack.push(Object::Null);
    }

    fn handle_op_eq(&mut self) {
        let b = pop!(self.stack);
        let a = pop!(self.stack);
        self.stack.push((a == b).into())
    }

    fn handle_op_lt(&mut self) -> Result<()> {
        binop_relational!(self, <);

        Ok(())
    }

    fn handle_op_gt(&mut self) -> Result<()> {
        binop_relational!(self, >);

        Ok(())
    }

    fn handle_op_jmp(&mut self, addr: usize) {
        self.ip = addr;
    }

    fn handle_op_jz(&mut self, addr: usize) {
        let item = pop!(self.stack);
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
        let retaddr = unsafe { self.frame_ptrs.pop().unwrap_unchecked() };
        let InternalObject::BytecodePtr(ptr, _) = retaddr;
        self.ip = ptr;
    }

    fn handle_op_deepget(&mut self, idx: usize) {
        let item = self.stack.get(adjust_idx!(self, idx));
        self.stack.push(item.clone());
    }

    fn handle_op_deepgetptr(&mut self, idx: usize) {
        let item = self.stack.get_mut(adjust_idx!(self, idx));
        let ptr = item as *mut Object<'_>;
        self.stack.push(Object::Ptr(ptr));
    }

    fn handle_op_deepset(&mut self, idx: usize) {
        self.stack.data[adjust_idx!(self, idx)] = pop!(self.stack);
    }

    fn handle_op_deepsetderef(&mut self, idx: usize) -> Result<()> {
        let obj = pop!(self.stack);
        let target = self.stack.get_mut(adjust_idx!(self, idx));
        let pointer = if let Object::Ptr(ptr) = target {
            ptr
        } else {
            bail!("");
        };

        unsafe {
            **pointer = obj;
        }

        Ok(())
    }

    fn handle_op_deref(&mut self) -> Result<()> {
        match pop!(self.stack) {
            Object::Ptr(ptr) => self.stack.push(unsafe { (*ptr).clone() }),
            _ => bail!("vm: tried to deref a non-ptr"),
        }

        Ok(())
    }

    fn handle_op_getattr(&mut self, member: &str) -> Result<()> {
        if let Object::Struct(obj) = pop!(self.stack) {
            match obj.borrow().members.get(member) {
                Some(m) => self.stack.push(m.clone()),
                None => bail!(
                    "vm: struct '{}' has no member '{}'",
                    obj.borrow().name,
                    member
                ),
            };
        }

        Ok(())
    }

    fn handle_op_setattr(&mut self, member: &'src str) {
        let value = pop!(self.stack);
        let structobj = pop!(self.stack);
        if let Object::Struct(s) = structobj {
            s.borrow_mut().members.insert(member.into(), value);
            self.stack.push(Object::Struct(s));
        }
    }

    fn handle_op_struct(&mut self, name: &'src str) {
        let structobj = Object::Struct(Rc::new(
            (StructObject {
                members: HashMap::new(),
                name,
            })
            .into(),
        ));
        self.stack.push(structobj);
    }

    fn handle_op_pop(&mut self, popcount: usize) {
        for _ in 0..popcount {
            pop!(self.stack);
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object<'src> {
    Number(f64),
    Bool(bool),
    String(Rc<Cow<'src, str>>),
    Struct(Rc<RefCell<StructObject<'src>>>),
    Ptr(*mut Object<'src>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructObject<'src> {
    members: HashMap<Rc<str>, Object<'src>>,
    name: &'src str,
}

#[derive(Debug, Clone, Copy)]
enum InternalObject {
    BytecodePtr(usize, usize),
}

impl<'src> std::ops::Add for Object<'src> {
    type Output = Result<Object<'src>>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => Ok((a + b).into()),
            _ => bail!("vm: only numbers can be +"),
        }
    }
}

impl<'src> std::ops::Sub for Object<'src> {
    type Output = Result<Object<'src>>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => Ok((a - b).into()),
            _ => bail!("vm: only numbers can be -"),
        }
    }
}

impl<'src> std::ops::Mul for Object<'src> {
    type Output = Result<Object<'src>>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => Ok((a * b).into()),
            _ => bail!("vm: only numbers can be *"),
        }
    }
}

impl<'src> std::ops::Div for Object<'src> {
    type Output = Result<Object<'src>>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => Ok((a / b).into()),
            _ => bail!("vm: only numbers can be /"),
        }
    }
}

impl<'src> std::ops::Not for Object<'src> {
    type Output = Result<Object<'src>>;

    fn not(self) -> Self::Output {
        match self {
            Object::Bool(b) => Ok((!b).into()),
            _ => bail!("vm: only bools can be !"),
        }
    }
}

impl<'src> std::ops::Neg for Object<'src> {
    type Output = Result<Object<'src>>;

    fn neg(self) -> Self::Output {
        match self {
            Object::Number(b) => Ok((-b).into()),
            _ => bail!("vm: only numbers can be -"),
        }
    }
}

impl<'src> std::cmp::PartialOrd for Object<'src> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl<'src> From<bool> for Object<'src> {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl<'src> From<f64> for Object<'src> {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl<'src> From<String> for Object<'src> {
    fn from(value: String) -> Self {
        Self::String(Rc::new(Cow::Owned(value)))
    }
}

impl<'src> From<&'src str> for Object<'src> {
    fn from(value: &'src str) -> Self {
        Self::String(Rc::new(Cow::Borrowed(value)))
    }
}

#[derive(Debug)]
struct Stack<'src> {
    data: Box<[Object<'src>]>,
    tos: usize,
}

impl<'src> Stack<'src> {
    fn with_capacity(capacity: usize) -> Self {
        let mut vec = Vec::with_capacity(capacity);

        for _ in 0..capacity {
            vec.push(Object::Null);
        }

        let data = vec.into_boxed_slice();

        Self { data, tos: 0 }
    }

    fn push(&mut self, item: Object<'src>) {
        self.data[self.tos] = item;
        self.tos += 1;
    }

    fn pop(&mut self) -> Object<'src> {
        self.tos -= 1;
        std::mem::replace(
            unsafe { self.data.get_unchecked_mut(self.tos) },
            Object::Null,
        )
    }

    fn len(&self) -> usize {
        self.tos
    }

    fn get_mut(&mut self, n: usize) -> &mut Object<'src> {
        unsafe { self.data.get_unchecked_mut(n) }
    }

    fn get(&self, n: usize) -> &Object<'src> {
        unsafe { self.data.get_unchecked(n) }
    }
}
