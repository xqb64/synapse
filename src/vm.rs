use crate::compiler::{Blueprint, Bytecode, Function, Opcode};
use anyhow::{bail, Result};
use std::borrow::Cow;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

macro_rules! pop {
    ($stack:expr) => {{
        $stack.pop().unwrap()
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
        let BytecodePtr { ptr: _, location } = $self.frame_ptrs.last().unwrap();
        location + $index
    }};
}

macro_rules! match_opcode {
    ($opcode:expr ; $($name:ident => $op:expr),* $(,)?) => {{

        #[allow(non_upper_case_globals)]
        mod _consts {
            $(pub const $name: u8 = super::Opcode::$name as u8;)*
        }

        match $opcode {
            $(_consts::$name => $op,)*
            _ => panic!("Unexpected opcode"),
        }
    }};
}

pub struct VM<'src, 'bytecode> {
    bytecode: &'bytecode Bytecode<'src>,
    stack: Vec<Object<'src>>,
    frame_ptrs: Vec<BytecodePtr>,
    ip: usize,
    blueprints: HashMap<&'src str, Blueprint<'src>>,
}

const STACK_MIN: usize = 1024;

impl<'src, 'bytecode> VM<'src, 'bytecode>
where
    'bytecode: 'src,
{
    pub fn new(bytecode: &'bytecode Bytecode<'src>) -> VM<'src, 'bytecode> {
        VM {
            bytecode,
            stack: Vec::with_capacity(STACK_MIN),
            frame_ptrs: Vec::with_capacity(STACK_MIN),
            ip: 0,
            blueprints: HashMap::new(),
        }
    }

    pub fn exec(&mut self) -> Result<()> {
        loop {
            let opcode = self.bytecode.code[self.ip];

            if cfg!(debug_assertions) {
                println!("current instruction: {:?}", Opcode::from(opcode));
            }

            match_opcode!(opcode;
                Const => self.handle_op_const(),
                Str => self.handle_op_str(),
                Print => self.handle_op_print(),
                Add => self.handle_op_add()?,
                Sub => self.handle_op_sub()?,
                Mul => self.handle_op_mul()?,
                Div => self.handle_op_div()?,
                Mod => self.handle_op_mod()?,
                BitAnd => self.handle_op_bitand()?,
                BitOr => self.handle_op_bitor()?,
                BitXor => self.handle_op_bitxor()?,
                BitNot => self.handle_op_bitnot()?,
                BitShl => self.handle_op_bitshl()?,
                BitShr => self.handle_op_bitshr()?,
                False => self.handle_op_false(),
                Not => self.handle_op_not()?,
                Neg => self.handle_op_neg()?,
                Null => self.handle_op_null(),
                Eq => self.handle_op_eq(),
                Lt => self.handle_op_lt()?,
                Gt => self.handle_op_gt()?,
                Jmp => self.handle_op_jmp(),
                Jz => self.handle_op_jz(),
                Call => self.handle_op_call(),
                CallMethod => self.handle_op_call_method()?,
                Ret => self.handle_op_ret(),
                Deepget => self.handle_op_deepget(),
                DeepgetPtr => self.handle_op_deepgetptr(),
                Deepset => self.handle_op_deepset(),
                Deref => self.handle_op_deref()?,
                DerefSet => self.handle_op_derefset()?,
                Getattr => self.handle_op_getattr()?,
                GetattrPtr => self.handle_op_getattrptr()?,
                Setattr => self.handle_op_setattr(),
                Struct => self.handle_op_struct(),
                StructBlueprint => self.handle_op_struct_blueprint(),
                Impl => self.handle_op_impl(),
                Strcat => self.handle_op_strcat()?,
                Pop => self.handle_op_pop(),
                Halt => break,
                Panic => panic!("vm: raw byte"),
            );

            if cfg!(debug_assertions) {
                self.print_stack();
            }

            self.ip += 1;
        }

        Ok(())
    }

    fn print_stack(&self) {
        print!("stack: [");
        for (i, item) in self.stack.iter().enumerate() {
            print!("{:?}", item);
            if i < self.stack.len() - 1 {
                print!(", ");
            }
        }
        println!("]");
    }

    #[inline(always)]
    fn read_u32(&mut self) -> u32 {
        let bytes = &self.bytecode.code[self.ip + 1..self.ip + 5];
        let n = u32::from_be_bytes(bytes.try_into().unwrap());

        self.ip += 4;

        n
    }

    /// Handles 'Opcode::Const(f64)' by constructing
    /// an Object::Number, with the f64 as its value,
    /// and pushing it on the stack.
    fn handle_op_const(&mut self) {
        let n = self.bytecode.cp[self.read_u32() as usize];
        self.stack.push(n.into());
    }

    /// Handles 'Opcode::Str(&str)' by constructing
    /// an Object::String, with the &str as its va-
    /// lue, and pushing it on the stack.
    fn handle_op_str(&mut self) {
        let s = self.bytecode.sp[self.read_u32() as usize];
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
    fn handle_op_jmp(&mut self) {
        self.ip = self.read_u32() as usize;
    }

    /// Handles 'Opcode::Jz(usize)' by popping an
    /// object off the stack (expected to be bool),
    /// and setting the instruction pointer to the
    /// address provided in the opcode, if and only
    /// if the popped object was falsey.
    fn handle_op_jz(&mut self) {
        let addr = self.read_u32() as usize;
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
    fn handle_op_call(&mut self) {
        let n = self.read_u32() as usize;
        self.frame_ptrs.push(BytecodePtr {
            ptr: self.ip + 5,
            location: self.stack.len() - n,
        });
    }

    fn handle_op_call_method(&mut self) -> Result<()> {
        let object = self.stack.last().unwrap();

        let object_type = if let Object::Struct(structobj) = object {
            structobj.borrow().name
        } else {
            bail!("vm: tried to call a method on a non-struct");
        };

        let name = self.bytecode.sp[self.read_u32() as usize];

        if let Some(blueprint) = self.blueprints.get(object_type) {
            if let Some(method) = blueprint.methods.get(name) {
                self.frame_ptrs.push(BytecodePtr {
                    ptr: self.ip,
                    location: self.stack.len() - method.paramcount,
                });

                self.ip = method.location - 1;
            } else {
                bail!("vm: struct '{}' has no method '{}'", object_type, name);
            }
        } else {
            bail!("vm: struct '{}' is not defined", object_type);
        }

        Ok(())
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
    fn handle_op_deepget(&mut self) {
        let idx = self.read_u32() as usize;
        let obj = self.stack.get(adjust_idx!(self, idx)).unwrap();
        self.stack.push(obj.clone());
    }

    /// Handles 'Opcode::DeepgetPtr(usize)' by getting
    /// the pointer to the object at index 'idx' (rel-
    /// ative to the current frame pointer), and push-
    /// ing it on the stack.
    fn handle_op_deepgetptr(&mut self) {
        let idx = self.read_u32() as usize;
        let adjusted_idx = adjust_idx!(self, idx);
        let obj = self.stack.get(adjusted_idx).unwrap();
        let rc = Rc::new(RefCell::new(obj.clone()));
        if let Object::Ref(obj) = obj {
            self.stack.push(Object::Ptr(obj.clone()));
        } else {
            self.stack.push(Object::Ptr(rc.clone()));
            self.stack[adjusted_idx] = Object::Ref(rc);
        }
    }

    /// Handles 'Opcode::Deepset(usize)' by popping an
    /// object off the stack and setting the object at
    /// index 'idx' (relative to the current frame po-
    /// inter) to the popped object.
    fn handle_op_deepset(&mut self) {
        let idx = self.read_u32() as usize;
        self.stack.swap_remove(adjust_idx!(self, idx));
    }

    /// Handles 'Opcode::Deref' by popping an object off
    /// the stack, dereferencing it, and pushing the re-
    /// sult back on the stack.
    fn handle_op_deref(&mut self) -> Result<()> {
        match pop!(self.stack) {
            Object::Ptr(ptr) => self.stack.push(ptr.borrow().clone()),
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
                let mut p = ptr.borrow_mut();
                *p = item;
            }
            _ => bail!("vm: tried to deref a non-ptr"),
        }

        Ok(())
    }

    /// Handles 'Opcode::Getattr(&str)' by popping an object
    /// off the stack (expected to be a struct), looking up the
    /// member with the &str value contained in the opcode, and
    /// pushing it on the stack.
    fn handle_op_getattr(&mut self) -> Result<()> {
        let member = self.bytecode.sp[self.read_u32() as usize];

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
    fn handle_op_getattrptr(&mut self) -> Result<()> {
        let member = self.bytecode.sp[self.read_u32() as usize];

        if let Object::Struct(obj) = pop!(self.stack) {
            match obj.borrow_mut().members.get_mut(member) {
                Some(m) => {
                    if let Object::Ref(obj) = m {
                        self.stack.push(Object::Ptr(obj.clone()));
                    } else {
                        self.stack
                            .push(Object::Ptr(Rc::new(RefCell::new(m.clone()))));
                        *m = Object::Ref(Rc::new(RefCell::new(m.clone())));
                    }
                }
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
    fn handle_op_setattr(&mut self) {
        let member = self.bytecode.sp[self.read_u32() as usize];

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
    fn handle_op_struct(&mut self) {
        let name = self.bytecode.sp[self.read_u32() as usize];
        let structobj = Object::Struct(Rc::new(
            (StructObject {
                members: HashMap::new(),
                name,
            })
            .into(),
        ));
        self.stack.push(structobj);
    }

    fn handle_op_struct_blueprint(&mut self) {
        let blueprint_name_idx = self.read_u32();
        let member_count = self.read_u32();

        let mut bp = Blueprint {
            name: self.bytecode.sp[blueprint_name_idx as usize],
            members: Vec::new(),
            methods: HashMap::new(),
        };

        for _ in 0..member_count {
            let member_name_idx = self.read_u32();
            let member_name = self.bytecode.sp[member_name_idx as usize];
            bp.members.push(member_name);
        }

        self.blueprints
            .insert(self.bytecode.sp[blueprint_name_idx as usize], bp);
    }

    fn handle_op_impl(&mut self) {
        let blueprint_name_idx = self.read_u32();
        let method_count = self.read_u32();

        for _ in 0..method_count {
            let method_name_idx = self.read_u32();
            let paramcount = self.read_u32();
            let location = self.read_u32();

            let f = Function {
                name: self.bytecode.sp[method_name_idx as usize],
                paramcount: paramcount as usize,
                location: location as usize,
                localscount: 0,
            };

            if let Some(bp) = self
                .blueprints
                .get_mut(self.bytecode.sp[blueprint_name_idx as usize])
            {
                bp.methods.insert(f.name, f);
            }
        }
    }

    /// Handles 'Opcode::Pop(usize)' by popping
    /// 'popcount' objects off of the stack.
    fn handle_op_pop(&mut self) {
        let popcount = self.read_u32() as usize;

        for _ in 0..popcount {
            pop!(self.stack);
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Object<'src> {
    Number(f64),
    Bool(bool),
    String(Rc<Cow<'src, str>>),
    Struct(Rc<RefCell<StructObject<'src>>>),
    Ref(Rc<RefCell<Object<'src>>>),
    Ptr(Rc<RefCell<Object<'src>>>),
    Null,
}

impl<'src> std::fmt::Debug for Object<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Number(n) => write!(f, "{}", n),
            Object::Bool(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, r#""{}""#, s),
            Object::Struct(s) => write!(f, "{:?}", s),
            Object::Ptr(p) => write!(f, "{:?}", p.borrow()),
            Object::Ref(r) => write!(f, "{:?}", r.borrow()),
            Object::Null => write!(f, "null"),
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct StructObject<'src> {
    members: HashMap<Rc<str>, Object<'src>>,
    name: &'src str,
}

impl<'src> std::fmt::Debug for StructObject<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{ ", self.name)?;

        for (i, (key, value)) in self.members.iter().enumerate() {
            write!(f, "{}: {:?}", key, value)?;
            if i < self.members.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, " }}")
    }
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
