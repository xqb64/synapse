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
    ($self:tt, $op:tt) => {{
        let b = pop!($self.stack);
        let a = pop!($self.stack);
        let res = (a $op b);
        match res {
            Ok(r) => $self.stack.push(r.into()),
            Err(e) => bail!(e),
        }
    }};
}

fn prepare4bitwise(a: f64, b: f64) -> (u64, u64) {
    let clamped_a = a.clamp(0f64, u64::MAX as f64);
    let clamped_b = b.clamp(0f64, u64::MAX as f64);

    (clamped_a as u64, clamped_b as u64)
}

macro_rules! binop_relational {
    ($self:tt, $op:tt) => {{
        let b = pop!($self.stack);
        let a = pop!($self.stack);
        if std::mem::discriminant(&a) != std::mem::discriminant(&b) {
            bail!("vm: only numbers can be: <, >, <=, >=");
        }
        $self.stack.push((a $op b).into());
    }};
}

macro_rules! adjust_idx {
    ($self:tt, $index:expr) => {{
        let BytecodePtr { ptr: _, location } = unsafe { *$self.frame_ptrs.last() };
        location + $index
    }};
}

pub struct VM<'src, 'bytecode> {
    bytecode: &'bytecode [Opcode],
    stack: Stack<Object<'src>>,
    frame_ptrs: Stack<BytecodePtr>,
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
            frame_ptrs: Stack::with_capacity(STACK_MIN),
            ip: 0,
        }
    }

    pub fn exec(&mut self) -> Result<()> {
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
                Opcode::BitAnd => self.handle_op_bitand()?,
                Opcode::BitOr => self.handle_op_bitor()?,
                Opcode::BitXor => self.handle_op_bitxor()?,
                Opcode::BitNot => self.handle_op_bitnot()?,
                Opcode::BitShl => self.handle_op_bitshl()?,
                Opcode::BitShr => self.handle_op_bitshr()?,
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

    /// Handles 'Opcode::Const(f64)' by constructing
    /// an Object::Number, with the f64 as its value,
    /// and pushing it on the stack.
    fn handle_op_const(&mut self, n: f64) {
        self.stack.push(n.into());
    }

    /// Handles 'Opcode::Str(&str)' by constructing
    /// an Object::String, with the &str as its va-
    /// lue, and pushing it on the stack.
    fn handle_op_str(&mut self, s: &'src str) {
        self.stack.push(s.into());
    }

    /// Handles 'Opcode::Strcat' by popping two obj-
    /// ects off the stack (expected to be strings),
    /// concatenating them into a new string object,
    /// and pushing the new object on the stack.
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

    /// Handles 'Opcode::Print' by popping an obj-
    /// ect off the stack and printing it out.
    fn handle_op_print(&mut self) {
        let obj = pop!(self.stack);
        if cfg!(debug_assertions) {
            print!("dbg: ");
        }
        println!("{:?}", obj);
    }

    /// Handles 'Opcode::Add' by popping two obj-
    /// ects off the stack, adding them together,
    /// and pushing the result back on the stack.
    fn handle_op_add(&mut self) -> Result<()> {
        binop_arithmetic!(self, +);

        Ok(())
    }

    /// Handles 'Opcode::Sub' by popping two obj-
    /// ects off the stack, subtracting them, and
    /// pushing the result back on the stack.
    fn handle_op_sub(&mut self) -> Result<()> {
        binop_arithmetic!(self, -);

        Ok(())
    }

    /// Handles 'Opcode::Mul' by popping two obj-
    /// ects off the stack, multiplying them, and
    /// pushing the result back on the stack.
    fn handle_op_mul(&mut self) -> Result<()> {
        binop_arithmetic!(self, *);

        Ok(())
    }

    /// Handles 'Opcode::Div' by popping two obj-
    /// ects off the stack, dividing them, and p-
    /// ushing the result back on the stack.
    fn handle_op_div(&mut self) -> Result<()> {
        binop_arithmetic!(self, /);

        Ok(())
    }

    /// Handles 'Opcode::Mod' by popping two obj-
    /// ects off the stack, mod-ing them, and pu-
    /// shing the result back on the stack.
    fn handle_op_mod(&mut self) -> Result<()> {
        binop_arithmetic!(self, %);

        Ok(())
    }

    /// Handles 'Opcode::BitAnd' by popping two obj-
    /// ects off the stack, bitwise-anding them, and
    /// pushing the result back on the stack.
    fn handle_op_bitand(&mut self) -> Result<()> {
        binop_arithmetic!(self, &);

        Ok(())
    }

    /// Handles 'Opcode::BitOr' by popping two obj-
    /// ects off the stack, bitwise-oring them, and
    /// pushing the result back on the stack.
    fn handle_op_bitor(&mut self) -> Result<()> {
        binop_arithmetic!(self, |);

        Ok(())
    }

    /// Handles 'Opcode::BitXor' by popping two obj-
    /// ects off the stack, bitwise-xoring them, and
    /// pushing the result back on the stack.
    fn handle_op_bitxor(&mut self) -> Result<()> {
        binop_arithmetic!(self, ^);

        Ok(())
    }

    /// Handles 'Opcode::BitShl' by popping two obje-
    /// cts off the stack, performing the bitwise shl
    /// operation on the first operand using the sec-
    /// ond operand as the shift amount, and pushing
    /// the result back on the stack.
    fn handle_op_bitshl(&mut self) -> Result<()> {
        binop_arithmetic!(self, <<);

        Ok(())
    }

    /// Handles 'Opcode::BitShr' by popping two obje-
    /// cts off the stack, performing the bitwise shr
    /// operation on the first operand using the sec-
    /// ond operand as the shift amount, and pushing
    /// the result back on the stack.
    fn handle_op_bitshr(&mut self) -> Result<()> {
        binop_arithmetic!(self, >>);

        Ok(())
    }

    /// Handles 'Opcode::BitNot' by popping an obje-
    /// ct off the stack, performing the bitwise not
    /// operation on it, and pushing the result back
    /// on the stack.
    fn handle_op_bitnot(&mut self) -> Result<()> {
        let obj = pop!(self.stack);
        self.stack.push((!obj)?);

        Ok(())
    }

    /// Handles 'Opcode::False' by constructing an
    /// Object::Bool, with false as its value, and
    /// pushing it on the stack.
    fn handle_op_false(&mut self) {
        self.stack.push(false.into());
    }

    /// Handles 'Opcode::Not' by popping an object
    /// off the stack, performing the logical not
    /// operation on it, and pushing the result back
    /// on the stack.
    fn handle_op_not(&mut self) -> Result<()> {
        let obj = pop!(self.stack);
        self.stack.push((!obj)?);

        Ok(())
    }

    /// Handles 'Opcode::Neg' by popping an object
    /// off the stack, performing the logical negate
    /// operation on it, and pushing the result back
    /// on the stack.
    fn handle_op_neg(&mut self) -> Result<()> {
        let obj = pop!(self.stack);
        self.stack.push((-obj)?);

        Ok(())
    }

    /// Handles 'Opcode::Null' by constructing an
    /// Object::Null and pushing it on the stack.
    fn handle_op_null(&mut self) {
        self.stack.push(Object::Null);
    }

    /// Handles 'Opcode::Eq' by popping two objects
    /// off the stack, performing the equality check
    /// on them, and pushing the boolean result back
    /// on the stack.
    fn handle_op_eq(&mut self) {
        let b = pop!(self.stack);
        let a = pop!(self.stack);
        self.stack.push((a == b).into())
    }

    /// Handles 'Opcode::Lt' by popping two objects
    /// off the stack, performing the less-than check
    /// on them, and pushing the boolean result back
    /// on the stack.
    fn handle_op_lt(&mut self) -> Result<()> {
        binop_relational!(self, <);

        Ok(())
    }

    /// Handles 'Opcode::Gt' by popping two objects
    /// off the stack, performing the greater-than
    /// check on them, and pushing the boolean result
    /// back on the stack.
    fn handle_op_gt(&mut self) -> Result<()> {
        binop_relational!(self, >);

        Ok(())
    }

    /// Handles 'Opcode::Jmp(usize)' by setting the
    /// instruction pointer to the address provided
    /// in the opcode.
    fn handle_op_jmp(&mut self, addr: usize) {
        self.ip = addr;
    }

    /// Handles 'Opcode::Jz(usize)' by popping an
    /// object off the stack (expected to be bool),
    /// and setting the instruction pointer to the
    /// address provided in the opcode, if and only
    /// if the popped object was falsey.
    fn handle_op_jz(&mut self, addr: usize) {
        let item = pop!(self.stack);
        if let Object::Bool(_b @ false) = item {
            self.ip = addr;
        }
    }

    /// Handles 'Opcode::Call(usize)' by pushing a
    /// BytecodePtr object on the frame ptr stack.
    /// The object will point to the next instruc-
    /// tion that comes after the current instruc-
    /// tion pointer, and its location will be the
    /// size of the stack - n.
    fn handle_op_call(&mut self, n: usize) {
        self.frame_ptrs.push(BytecodePtr {
            ptr: self.ip + 1,
            location: self.stack.len() - n,
        });
    }

    /// Handles 'Opcode::Ret' by popping a BytecodePtr
    /// object off of the frame ptr stack, and setting
    /// the instruction pointer to the address contai-
    /// ned within the object.
    fn handle_op_ret(&mut self) {
        let retaddr = pop!(self.frame_ptrs);
        let BytecodePtr { ptr, location: _ } = retaddr;
        self.ip = ptr;
    }

    /// Handles 'Opcode::Deepget(usize)' by getting an
    /// object at index 'idx' (relative to the current
    /// frame pointer), and pushing it on the stack.
    fn handle_op_deepget(&mut self, idx: usize) {
        let ptr = self.stack.get_raw(adjust_idx!(self, idx));
        self.stack.push(unsafe { (*ptr).clone() });
    }

    /// Handles 'Opcode::DeepgetPtr(usize)' by getting
    /// the pointer to the object at index 'idx' (rel-
    /// ative to the current frame pointer), and push-
    /// ing it on the stack.
    fn handle_op_deepgetptr(&mut self, idx: usize) {
        let ptr = self.stack.get_raw(adjust_idx!(self, idx));
        self.stack.push(Object::Ptr(ptr));
    }

    /// Handles 'Opcode::Deepset(usize)' by popping an
    /// object off the stack and setting the object at
    /// index 'idx' (relative to the current fp)to the
    /// popped object.
    fn handle_op_deepset(&mut self, idx: usize) {
        let ptr = self.stack.get_raw(adjust_idx!(self, idx));
        unsafe {
            *ptr = pop!(self.stack);
        }
    }

    /// Handles 'Opcode::Deref' by popping an object off
    /// the stack, dereferencing it, and pushing the re-
    /// sult back on the stack.
    fn handle_op_deref(&mut self) -> Result<()> {
        match pop!(self.stack) {
            Object::Ptr(ptr) => self.stack.push(unsafe { (*ptr).clone() }),
            _ => bail!("vm: tried to deref a non-ptr"),
        }

        Ok(())
    }

    /// Handles 'Opcode::Derefset' by popping two objects
    /// off the stack (the value and the pointer), deref-
    /// erencing the pointer, and setting it to the value.
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

    /// Handles 'Opcode::Getattr(&str)' by popping an object
    /// off the stack (expected to be a struct), looking up the
    /// member with the &str value contained in the opcode, and
    /// pushing it on the stack.
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

    /// Handles 'Opcode::GetattrPtr(&str)' by popping an object
    /// off the stack (expected to be a struct), looking up the
    /// member with the &str value contained in the opcode, and
    /// pushing the pointer to it on the stack.
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

    /// Handles 'Opcode::Setattr(&str)' by popping two objects
    /// off the stack (expected to be a value and a struct, re-
    /// spectively), setting the member with the &str value co-
    /// ntained in the opcode to the popped value, and pushing
    /// the struct back on the stack.
    fn handle_op_setattr(&mut self, member: &'src str) {
        let value = pop!(self.stack);
        let structobj = pop!(self.stack);
        if let Object::Struct(s) = structobj {
            s.borrow_mut().members.insert(member.into(), value);
            self.stack.push(Object::Struct(s));
        }
    }

    /// Handles 'Opcode::Struct(&str)' by constructing an
    /// Object::Struct (using the &str value contained in
    /// the opcode as the naame, and with an empty members
    /// HashMap), and pushing it on the stack.
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

    /// Handles 'Opcode::Pop(usize)' by popping
    /// 'popcount' objects off of the stack.
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

impl<'src> std::ops::BitAnd for Object<'src> {
    type Output = Result<Object<'src>>;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => {
                let (a, b) = prepare4bitwise(a, b);
                Ok(((a & b) as f64).into())
            }
            _ => bail!("vm: only numbers can be %"),
        }
    }
}

impl<'src> std::ops::BitOr for Object<'src> {
    type Output = Result<Object<'src>>;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => {
                let (a, b) = prepare4bitwise(a, b);
                Ok(((a | b) as f64).into())
            }
            _ => bail!("vm: only numbers can be %"),
        }
    }
}

impl<'src> std::ops::BitXor for Object<'src> {
    type Output = Result<Object<'src>>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => {
                let (a, b) = prepare4bitwise(a, b);
                Ok(((a ^ b) as f64).into())
            }
            _ => bail!("vm: only numbers can be %"),
        }
    }
}

impl<'src> std::ops::Shl for Object<'src> {
    type Output = Result<Object<'src>>;

    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => {
                let (a, b) = prepare4bitwise(a, b);
                Ok(((a << b) as f64).into())
            }
            _ => bail!("vm: only numbers can be %"),
        }
    }
}

impl<'src> std::ops::Shr for Object<'src> {
    type Output = Result<Object<'src>>;

    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Object::Number(a), Object::Number(b)) => {
                let (a, b) = prepare4bitwise(a, b);
                Ok(((a >> b) as f64).into())
            }
            _ => bail!("vm: only numbers can be %"),
        }
    }
}

impl<'src> std::ops::Not for Object<'src> {
    type Output = Result<Object<'src>>;

    fn not(self) -> Self::Output {
        match self {
            Object::Number(n) => {
                let truncated = n as u64;
                let reduced = (truncated % (1u64 << 32)) as u32;
                Ok(((!reduced) as f64).into())
            }
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

/// A fixed-size stack is needed because the stack
/// could contain Object::Ptr, which in turn could
/// point to other elements on the stack, effecti-
/// vely making the entire structure self-referen-
/// tial. With this stack, we prevent reallocation
/// which would be bound to happen had we used the
/// built-in Vec, and hence the pointers never get
/// invalidated. The alternative was to use Pin to
/// pin the stack and the objects it contains, but
/// this was turning the whole codebase into a gi-
/// ant mess, so I wrote a stack that doesn't grow
#[derive(Debug)]
struct Stack<T> {
    data: *mut T,
    tos: usize,
    capacity: usize,
}

impl<T> Stack<T>
where
    T: std::fmt::Debug,
{
    fn with_capacity(capacity: usize) -> Self {
        use std::mem::ManuallyDrop;

        let mut vec = ManuallyDrop::new(Vec::with_capacity(capacity));

        Self {
            data: vec.as_mut_ptr(),
            tos: 0,
            capacity,
        }
    }

    fn push(&mut self, item: T) {
        assert!(self.tos < self.capacity, "stack overflow");
        unsafe {
            let ptr = self.data.add(self.tos);
            ptr.write(item);
        }
        self.tos += 1;
    }

    fn pop(&mut self) -> T {
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

    fn get_raw(&mut self, n: usize) -> *mut T {
        assert!(n <= self.tos, "tried to access element beyond tos");
        unsafe { self.data.add(n) }
    }

    fn last(&mut self) -> *mut T {
        assert!(self.tos > 0, "no elements on the stack");
        unsafe { self.data.add(self.tos - 1) }
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

impl<T> Drop for Stack<T> {
    fn drop(&mut self) {
        unsafe {
            let _vec = Vec::from_raw_parts(self.data, self.tos, self.capacity);
        }
    }
}
