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
        let BytecodePtr { ptr: _, location } = $self.frame_ptrs.last().unwrap();
        location + $index
    }};
}

pub struct VM<'src, 'bytecode> {
    bytecode: &'bytecode [Opcode],
    stack: Stack<'src>,
    frame_ptrs: Vec<BytecodePtr>,
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
                Opcode::Mod => self.handle_op_mod()?,
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
                Opcode::Deref => self.handle_op_deref()?,
                Opcode::DerefSet => self.handle_op_derefset()?,
                Opcode::Getattr(member) => self.handle_op_getattr(member)?,
                Opcode::GetattrPtr(member) => self.handle_op_getattrptr(member)?,
                Opcode::Setattr(member) => self.handle_op_setattr(member),
                Opcode::Struct(name) => self.handle_op_struct(name),
                Opcode::Strcat => self.handle_op_strcat()?,
                Opcode::Pop(popcount) => self.handle_op_pop(*popcount),
                Opcode::Halt => break Ok(()),
            }

            if cfg!(debug_assertions) {
                self.stack.print_elements();
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

    fn handle_op_mod(&mut self) -> Result<()> {
        binop_arithmetic!(self, %);

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
        self.frame_ptrs.push(BytecodePtr {
            ptr: self.ip + 1,
            location: self.stack.len() - n,
        });
    }

    fn handle_op_ret(&mut self) {
        let retaddr = self.frame_ptrs.pop().unwrap();
        let BytecodePtr { ptr, location: _ } = retaddr;
        self.ip = ptr;
    }

    fn handle_op_deepget(&mut self, idx: usize) {
        let ptr = self.stack.get_raw(adjust_idx!(self, idx));
        self.stack.push(unsafe { (*ptr).clone() });
    }

    fn handle_op_deepgetptr(&mut self, idx: usize) {
        let ptr = self.stack.get_raw(adjust_idx!(self, idx));
        self.stack.push(Object::Ptr(ptr));
    }

    fn handle_op_deepset(&mut self, idx: usize) {
        let ptr = self.stack.get_raw(adjust_idx!(self, idx));
        unsafe {
            *ptr = pop!(self.stack);
        }
    }

    fn handle_op_deref(&mut self) -> Result<()> {
        match pop!(self.stack) {
            Object::Ptr(ptr) => self.stack.push(unsafe { (*ptr).clone() }),
            _ => bail!("vm: tried to deref a non-ptr"),
        }

        Ok(())
    }

    fn handle_op_derefset(&mut self) -> Result<()> {
        let item = pop!(self.stack);
        match pop!(self.stack) {
            Object::Ptr(ptr) => {
                unsafe { *ptr = item };
            }
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

    fn handle_op_getattrptr(&mut self, member: &str) -> Result<()> {
        if let Object::Struct(obj) = pop!(self.stack) {
            match obj.borrow_mut().members.get_mut(member) {
                Some(m) => self.stack.push(Object::Ptr(m as *mut Object<'src>)),
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

#[derive(Debug, Copy, Clone)]
pub struct BytecodePtr {
    ptr: usize,
    location: usize,
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

impl<'src> std::ops::Rem for Object<'src> {
    type Output = Result<Object<'src>>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => Ok((a % b).into()),
            _ => bail!("vm: only numbers can be %"),
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
    data: *mut Object<'src>,
    tos: usize,
    capacity: usize,
}

impl<'src> Stack<'src> {
    fn with_capacity(capacity: usize) -> Self {
        use std::mem::ManuallyDrop;

        let mut vec = ManuallyDrop::new(Vec::with_capacity(capacity));

        Self {
            data: vec.as_mut_ptr(),
            tos: 0,
            capacity,
        }
    }

    fn push(&mut self, item: Object<'src>) {
        assert!(self.tos < self.capacity, "stack overflow");
        unsafe {
            let ptr = self.data.add(self.tos);
            ptr.write(item);
        }
        self.tos += 1;
    }

    fn pop(&mut self) -> Object<'src> {
        assert!(self.tos > 0, "popped an empty stack");
        self.tos -= 1;
        unsafe {
            let ptr = self.data.add(self.tos);
            ptr.read()
        }
    }

    fn len(&self) -> usize {
        self.tos
    }

    fn get_raw(&mut self, n: usize) -> *mut Object<'src> {
        assert!(n <= self.tos, "tried to access element beyond tos");
        unsafe { self.data.add(n) }
    }

    fn print_elements(&self) {
        print!("stack: [");
        let mut current = self.data;
        for n in 0..self.tos {
            print!("{:?}", unsafe { &*current });
            if n < self.tos - 1 {
                print!(", ");
            }
            current = unsafe { current.add(1) };
        }
        println!("]");
    }
}

impl<'src> Drop for Stack<'src> {
    fn drop(&mut self) {
        unsafe {
            let _vec = Vec::from_raw_parts(self.data, self.tos, self.capacity);
        }
    }
}
