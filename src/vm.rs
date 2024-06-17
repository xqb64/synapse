use crate::compiler::{Blueprint, Bytecode, Function, Opcode};
use anyhow::{anyhow, bail, Result};
use std::borrow::Cow;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

macro_rules! binop_arithmetic {
    ($self:tt, $op:tt) => {{
        let b = $self.stack.pop();
        let a = $self.stack.pop();
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
        let b = $self.stack.pop();
        let a = $self.stack.pop();
        if std::mem::discriminant(&a) != std::mem::discriminant(&b) {
            bail!("vm: only numbers can be: <, >, <=, >=");
        }
        $self.stack.push((a $op b).into());
    }};
}

macro_rules! adjust_idx {
    ($self:tt, $index:expr) => {{
        let BytecodePtr { ptr: _, location } = *$self.frame_ptrs.last();
        location + $index
    }};
}

pub struct VM<'src, 'bytecode> {
    bytecode: &'bytecode Bytecode<'src>,
    stack: Stack<Object<'src>>,
    frame_ptrs: Stack<BytecodePtr>,
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
            stack: Stack::new(),
            frame_ptrs: Stack::new(),
            ip: 0,
            blueprints: HashMap::new(),
        }
    }

    pub fn exec(&mut self) -> Result<()> {
        loop {
            let opcode = Opcode::from(unsafe { *self.bytecode.code.get_unchecked(self.ip) });

            if cfg!(debug_assertions) {
                println!("current instruction: {:?}", opcode);
            }

            match opcode {
                Opcode::Const => self.handle_op_const(),
                Opcode::Str => self.handle_op_str(),
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
                Opcode::Jmp => self.handle_op_jmp(),
                Opcode::Jz => self.handle_op_jz(),
                Opcode::Call => self.handle_op_call(),
                Opcode::CallMethod => self.handle_op_call_method()?,
                Opcode::Ret => self.handle_op_ret(),
                Opcode::Deepget => self.handle_op_deepget(),
                Opcode::DeepgetPtr => self.handle_op_deepgetptr(),
                Opcode::Deepset => self.handle_op_deepset(),
                Opcode::Deref => self.handle_op_deref()?,
                Opcode::DerefSet => self.handle_op_derefset()?,
                Opcode::Getattr => self.handle_op_getattr()?,
                Opcode::GetattrPtr => self.handle_op_getattrptr()?,
                Opcode::Setattr => self.handle_op_setattr(),
                Opcode::Struct => self.handle_op_struct(),
                Opcode::StructBlueprint => self.handle_op_struct_blueprint()?,
                Opcode::Impl => self.handle_op_impl()?,
                Opcode::Strcat => self.handle_op_strcat()?,
                Opcode::Vec => self.handle_op_vec(),
                Opcode::VecSet => self.handle_op_vec_set(),
                Opcode::Subscript => self.handle_op_subscript(),
                Opcode::Pop => self.handle_op_pop(),
                Opcode::Halt => break Ok(()),
                Opcode::Raw => panic!("vm: raw byte"),
            }

            if cfg!(debug_assertions) {
                self.stack.print_elements();
            }

            self.ip += 1;
        }
    }

    fn read_u32(&mut self) -> u32 {
        let value = unsafe {
            let ptr = self.bytecode.code.as_ptr().add(self.ip + 1);
            let u32_ptr = ptr as *const [u8; 4];

            std::ptr::read_unaligned(u32_ptr)
        };

        self.ip += 4;

        u32::from_be_bytes(value)
    }

    /// Handles 'Opcode::Const(f64)' by constructing
    /// an Object::Number, with the f64 as its value,
    /// and pushing it on the stack.
    fn handle_op_const(&mut self) {
        let idx = self.read_u32();
        let n = unsafe { self.bytecode.cp.get_unchecked(idx as usize) };
        self.stack.push((*n).into());
    }

    /// Handles 'Opcode::Str(&str)' by constructing
    /// an Object::String, with the &str as its va-
    /// lue, and pushing it on the stack.
    fn handle_op_str(&mut self) {
        let idx = self.read_u32();
        let s = unsafe { self.bytecode.sp.get_unchecked(idx as usize) };
        self.stack.push((*s).into());
    }

    /// Handles 'Opcode::Strcat' by popping two obj-
    /// ects off the stack (expected to be strings),
    /// concatenating them into a new string object,
    /// and pushing the new object on the stack.
    fn handle_op_strcat(&mut self) -> Result<()> {
        let b = self.stack.pop();
        let a = self.stack.pop();

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
        let obj = self.stack.pop();
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
        let obj = self.stack.pop();
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
        let obj = self.stack.pop();
        self.stack.push((!obj)?);

        Ok(())
    }

    /// Handles 'Opcode::Neg' by popping an object
    /// off the stack, performing the logical negate
    /// operation on it, and pushing the result back
    /// on the stack.
    fn handle_op_neg(&mut self) -> Result<()> {
        let obj = self.stack.pop();
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
        let b = self.stack.pop();
        let a = self.stack.pop();
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
        let addr = self.read_u32();
        self.ip = addr as usize;
    }

    /// Handles 'Opcode::Jz(usize)' by popping an
    /// object off the stack (expected to be bool),
    /// and setting the instruction pointer to the
    /// address provided in the opcode, if and only
    /// if the popped object was falsey.
    fn handle_op_jz(&mut self) {
        let addr = self.read_u32();
        let item = self.stack.pop();
        if let Object::Bool(_b @ false) = item {
            self.ip = addr as usize;
        }
    }

    /// Handles 'Opcode::Call(usize)' by pushing a
    /// BytecodePtr object on the frame ptr stack.
    /// The object will point to the next instruc-
    /// tion that comes after the current instruc-
    /// tion pointer, and its location will be the
    /// size of the stack - n.
    fn handle_op_call(&mut self) {
        let n = self.read_u32();
        self.frame_ptrs.push(BytecodePtr {
            ptr: self.ip + 5,
            location: self.stack.len() - n as usize,
        });
    }

    fn handle_op_call_method(&mut self) -> Result<()> {
        let method_name_idx = self.read_u32();
        let argcount = self.read_u32();

        let object = self.stack.peek(argcount as usize);

        let object_type = if let Object::Struct(structobj) = object {
            structobj.borrow().name
        } else {
            bail!("vm: tried to call a method on a non-struct");
        };

        // It's safe to .unwrap() here because the blueprint must have been defined already.
        let blueprint = self.blueprints.get(object_type).unwrap();

        let method_name = self.bytecode.sp[method_name_idx as usize];

        if let Some(method) = blueprint.methods.get(method_name) {
            if argcount as usize != method.paramcount - 1 {
                bail!(
                    "vm: method '{}' expects {} arguments, got {}",
                    method.name,
                    method.paramcount - 1,
                    argcount
                );
            }

            self.frame_ptrs.push(BytecodePtr {
                ptr: self.ip,
                location: self.stack.len() - method.paramcount,
            });

            self.ip = method.location;
        } else {
            bail!(
                "vm: struct '{}' has no method '{}'",
                object_type,
                method_name
            );
        }

        Ok(())
    }

    /// Handles 'Opcode::Ret' by popping a BytecodePtr
    /// object off of the frame ptr stack, and setting
    /// the instruction pointer to the address contai-
    /// ned within the object.
    fn handle_op_ret(&mut self) {
        let retaddr = self.frame_ptrs.pop();
        let BytecodePtr { ptr, location: _ } = retaddr;
        self.ip = ptr;
    }

    /// Handles 'Opcode::Deepget(usize)' by getting an
    /// object at index 'idx' (relative to the current
    /// frame pointer), and pushing it on the stack.
    fn handle_op_deepget(&mut self) {
        let idx = self.read_u32() as usize;
        let obj = unsafe {
            self.stack
                .data
                .get_unchecked_mut(adjust_idx!(self, idx))
                .clone()
        };
        self.stack.push(obj);
    }

    /// Handles 'Opcode::DeepgetPtr(usize)' by getting
    /// the pointer to the object at index 'idx' (rel-
    /// ative to the current frame pointer), and push-
    /// ing it on the stack.
    fn handle_op_deepgetptr(&mut self) {
        let idx = self.read_u32() as usize;
        let obj = &mut self.stack.data[adjust_idx!(self, idx)] as *mut Object<'src>;
        self.stack.push(Object::Ptr(obj));
    }

    /// Handles 'Opcode::Deepset(usize)' by popping an
    /// object off the stack and setting the object at
    /// index 'idx' (relative to the current frame po-
    /// inter) to the popped object.
    fn handle_op_deepset(&mut self) {
        let idx = self.read_u32() as usize;
        self.stack.data.swap_remove(adjust_idx!(self, idx));
    }

    /// Handles 'Opcode::Deref' by popping an object off
    /// the stack, dereferencing it, and pushing the re-
    /// sult back on the stack.
    fn handle_op_deref(&mut self) -> Result<()> {
        match self.stack.pop() {
            Object::Ptr(ptr) => self.stack.push(unsafe { (*ptr).clone() }),
            _ => bail!("vm: tried to deref a non-ptr"),
        }

        Ok(())
    }

    /// Handles 'Opcode::Derefset' by popping two objects
    /// off the stack (the value and the pointer), deref-
    /// erencing the pointer, and setting it to the value.
    fn handle_op_derefset(&mut self) -> Result<()> {
        let item = self.stack.pop();
        match self.stack.pop() {
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
    fn handle_op_getattr(&mut self) -> Result<()> {
        let idx = self.read_u32() as usize;
        let attr = unsafe { self.bytecode.sp.get_unchecked(idx) };
        if let Object::Struct(obj) = self.stack.pop() {
            match obj.borrow().members.get(attr) {
                Some(m) => self.stack.push(m.clone()),
                None => bail!(
                    "vm: struct '{}' has no member '{}'",
                    obj.borrow().name,
                    attr
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
        let idx = self.read_u32() as usize;
        let attr = unsafe { self.bytecode.sp.get_unchecked(idx) };
        if let Object::Struct(obj) = self.stack.pop() {
            match obj.borrow_mut().members.get_mut(attr) {
                Some(m) => self.stack.push(Object::Ptr(m as *mut Object<'src>)),
                None => bail!(
                    "vm: struct '{}' has no member '{}'",
                    obj.borrow().name,
                    attr
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
        let idx = self.read_u32() as usize;
        let attr = unsafe { self.bytecode.sp.get_unchecked(idx) };
        let value = self.stack.pop();
        let structobj = self.stack.pop();
        if let Object::Struct(s) = structobj {
            s.borrow_mut().members.insert(attr, value);
            self.stack.push(Object::Struct(s));
        }
    }

    /// Handles 'Opcode::Struct(&str)' by constructing an
    /// Object::Struct (using the &str value contained in
    /// the opcode as the naame, and with an empty members
    /// HashMap), and pushing it on the stack.
    fn handle_op_struct(&mut self) {
        let idx = self.read_u32() as usize;
        let name = unsafe { self.bytecode.sp.get_unchecked(idx) };

        let structobj = Object::Struct(Rc::new(
            (StructObject {
                members: HashMap::new(),
                name,
            })
            .into(),
        ));
        self.stack.push(structobj);
    }

    fn handle_op_struct_blueprint(&mut self) -> Result<()> {
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

        Ok(())
    }

    fn handle_op_impl(&mut self) -> Result<()> {
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

        Ok(())
    }

    fn handle_op_vec(&mut self) {
        let element_count = self.read_u32() as usize;

        let mut vec = Vec::new();
        for _ in 0..element_count {
            vec.push(self.stack.pop());
        }
        self.stack.push(vec.into());
    }

    fn handle_op_vec_set(&mut self) {
        let value = self.stack.pop();
        let idx = self.stack.pop();
        let vec = self.stack.pop();

        if let Object::Vec(vec) = vec {
            if let Object::Number(idx) = idx {
                vec.borrow_mut()[idx as usize] = value;
            }
        }
    }

    fn handle_op_subscript(&mut self) {
        let idx = self.stack.pop();
        let vec = self.stack.pop();

        if let Object::Vec(vec) = vec {
            if let Object::Number(idx) = idx {
                self.stack.push(vec.borrow()[idx as usize].clone());
            }
        }
    }

    /// Handles 'Opcode::Pop(usize)' by popping
    /// 'popcount' objects off of the stack.
    fn handle_op_pop(&mut self) {
        let popcount = self.read_u32() as usize;
        for _ in 0..popcount {
            self.stack.pop();
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
    Vec(Rc<RefCell<Vec<Object<'src>>>>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructObject<'src> {
    members: HashMap<&'src str, Object<'src>>,
    name: &'src str,
}

#[derive(Debug, Default, Copy, Clone)]
pub struct BytecodePtr {
    ptr: usize,
    location: usize,
}

impl<'src> std::default::Default for Object<'src> {
    fn default() -> Self {
        Self::Null
    }
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

impl<'src> From<Vec<Object<'src>>> for Object<'src> {
    fn from(value: Vec<Object<'src>>) -> Self {
        Self::Vec(Rc::new(RefCell::new(value)))
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
    data: Vec<T>,
}

impl<T> Stack<T>
where
    T: std::fmt::Debug + std::default::Default + Clone,
{
    fn new() -> Stack<T> {
        Stack {
            data: Vec::with_capacity(STACK_MIN),
        }
    }

    fn push(&mut self, item: T) {
        assert!(self.data.len() < self.data.capacity(), "stack overflow");
        self.data.push(item);
    }

    fn pop(&mut self) -> T {
        assert!(!self.data.is_empty(), "popped an empty stack");
        unsafe { self.data.pop().unwrap_unchecked() }
    }

    fn peek(&mut self, n: usize) -> &T {
        assert!(!self.data.is_empty(), "peeked an empty stack");
        unsafe { self.data.get_unchecked(self.data.len() - 1 - n) }
    }

    fn len(&self) -> usize {
        self.data.len()
    }

    fn last(&mut self) -> &T {
        assert!(!self.data.is_empty(), "no elements on the stack");
        unsafe { self.data.last().unwrap_unchecked() }
    }

    fn print_elements(&self) {
        print!("stack: [");
        for (idx, n) in self.data.iter().enumerate() {
            print!("{:?}", n);
            if idx < self.data.len() - 1 {
                print!(", ");
            }
        }
        println!("]");
    }
}
