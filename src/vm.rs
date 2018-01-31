use std::fmt;

use num_traits::{ PrimInt, WrappingAdd, WrappingSub, WrappingMul };

use compiler::{ JumpOffset, LocalId };
use compiler::Function;
use compiler::CodeOffset;
use utils::id::{ IdVec, Id };

#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    IntegerBinary {
        op: IntegerBinaryOp,
        ty: PrimitiveType,
    },
    IntegerUnary {
        op: IntegerUnaryOp,
        ty: PrimitiveType,
    },
    PushConst(Primitive),
    LoadLocal(LocalId),
    StoreLocal(LocalId),
    Pop,
    Dup,
    BranchTrue(JumpOffset),
    BranchFalse(JumpOffset),
    Jump(JumpOffset),
    Return,
}

#[derive(Clone, Copy, Debug)]
pub enum IntegerBinaryOp {
    // Comparison
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Remainder,

    // Bitwise
    Or,
    Xor,
    LShift,
    RShift,
    And,
    AndNot,
}

impl IntegerBinaryOp {
    fn execute<T>(self, a: T, b: T) -> T
    where T: PrimInt + WrappingAdd + WrappingSub + WrappingMul
    {
        use self::IntegerBinaryOp::*;
        match self {
            Equal => T::from((a == b) as u8).unwrap(),
            NotEqual => T::from((a != b) as u8).unwrap(),
            Less => T::from((a <  b) as u8).unwrap(),
            LessOrEqual => T::from((a <= b) as u8).unwrap(),
            Greater => T::from((a >  b) as u8).unwrap(),
            GreaterOrEqual => T::from((a >= b) as u8).unwrap(),
            Add => a.wrapping_add(&b),
            Sub => a.wrapping_sub(&b),
            Or  => a | b,
            Xor => a ^ b,
            Mul => a.wrapping_mul(&b),
            Div => a.checked_div(&b).expect("Divide by zero unhandled"),
            Remainder => a % b,
            LShift => a << b.to_usize().unwrap(),
            RShift => a >> b.to_usize().unwrap(),
            And => a & b,
            AndNot => a & !b,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum IntegerUnaryOp {
    Minus,   // -
    LogNot,  // !
    Not,     // ^
}

impl IntegerUnaryOp {
    fn execute<T: PrimInt>(self, a: T) -> T {
        use self::IntegerUnaryOp::*;
        match self {
            Minus => !a + T::one(),
            LogNot => T::from(!(a == T::one()) as u8).unwrap(),
            Not => !a,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrimitiveType {
    U8,
    U16,
    U32,
    U64,

    I8,
    I16,
    I32,
    I64,
}

#[derive(Clone, Copy)]
pub union Primitive {
    pub u8: u8,
    pub u16: u16,
    pub u32: u32,
    pub u64: u64,

    pub i8: i8,
    pub i16: i16,
    pub i32: i32,
    pub i64: i64,
}

impl Default for Primitive {
    fn default() -> Primitive {
        unsafe {
            Primitive { u64: 0 }
        }
    }
}

impl fmt::Debug for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Primitive")
            .field(unsafe { &self.u64 })
            .finish()
    }
}

impl Primitive {
    pub fn load(ptr: *const (), ty: PrimitiveType) -> Primitive {
        use self::PrimitiveType::*;

        unsafe fn read<T: PrimInt>(src: *const T) -> T {
            use std::ptr;
            use std::mem;
            debug_assert!((src as usize) & (mem::align_of::<T>() - 1) == 0);
            ptr::read(src)
        }

        unsafe {
            match ty {
                U8 => Primitive { u8: read(ptr as *const u8) },
                U16 => Primitive { u16: read(ptr as *const u16) },
                U32 => Primitive { u32: read(ptr as *const u32) },
                U64 => Primitive { u64: read(ptr as *const u64) },

                I8 => Primitive { i8: read(ptr as *const i8) },
                I16 => Primitive { i16: read(ptr as *const i16) },
                I32 => Primitive { i32: read(ptr as *const i32) },
                I64 => Primitive { i64: read(ptr as *const i64) },
            }
        }
    }

    pub fn store(&self, ptr: *mut (), ty: PrimitiveType) {
        use self::PrimitiveType::*;

        unsafe fn write<T: PrimInt>(dst: *mut T, src: T) {
            use std::ptr;
            use std::mem;
            debug_assert!((dst as usize) & (mem::align_of::<T>() - 1) == 0);
            ptr::write(dst, src)
        }

        unsafe {
            match ty {
                U8 => write(ptr as *mut u8,self.u8),
                U16 => write(ptr as *mut u16, self.u16),
                U32 => write(ptr as *mut u32, self.u32),
                U64 => write(ptr as *mut u64, self.u64),

                I8 => write(ptr as *mut i8, self.i8),
                I16 => write(ptr as *mut i16, self.i16),
                I32 => write(ptr as *mut i32, self.i32),
                I64 => write(ptr as *mut i64, self.i64),
            }
        }
    }
}

fn integer_binary_execute(a: Primitive, b: Primitive, op: IntegerBinaryOp, ty: PrimitiveType) -> Primitive {
    use self::PrimitiveType::*;
    unsafe {
        match ty {
            U8  => Primitive { u8:  op.execute(a.u8,  b.u8) },
            U16 => Primitive { u16: op.execute(a.u16, b.u16) },
            U32 => Primitive { u32: op.execute(a.u32, b.u32) },
            U64 => Primitive { u64: op.execute(a.u64, b.u64) },

            I8  => Primitive { i8:  op.execute(a.i8,  b.i8) },
            I16 => Primitive { i16: op.execute(a.i16, b.i16) },
            I32 => Primitive { i32: op.execute(a.i32, b.i32) },
            I64 => Primitive { i64: op.execute(a.i64, b.i64) },
        }
    }
}

fn integer_unary_execute(a: Primitive, op: IntegerUnaryOp, ty: PrimitiveType) -> Primitive {
    use self::PrimitiveType::*;
    unsafe {
        match ty {
            U8  => Primitive { u8:  op.execute(a.u8) },
            U16 => Primitive { u16: op.execute(a.u16) },
            U32 => Primitive { u32: op.execute(a.u32) },
            U64 => Primitive { u64: op.execute(a.u64) },

            I8  => Primitive { i8:  op.execute(a.i8) },
            I16 => Primitive { i16: op.execute(a.i16) },
            I32 => Primitive { i32: op.execute(a.i32) },
            I64 => Primitive { i64: op.execute(a.i64) },
        }
    }
}

pub struct VirtualMachine {
    func: Function,
    stack: Vec<Primitive>,
    locals: IdVec<LocalId, Primitive>,
    pc: CodeOffset,
}

impl VirtualMachine {
    pub fn new(func: Function) -> VirtualMachine {
        let locals_count = func.local_names.len();
        VirtualMachine {
            func,
            stack: Vec::new(),
            locals: IdVec::with_len(locals_count),
            pc: CodeOffset::from_usize(0),
        }
    }

    pub fn display_locals(&self) {
        for (id, name) in self.func.local_names.iter() {
            println!("{} = {}", name, unsafe { self.locals[id].i64 });
        }
    }

    pub fn execute(&mut self) {
        loop {
            let opcode = self.func.code[self.pc];
            match opcode {
                Opcode::IntegerBinary { op, ty } => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let res = integer_binary_execute(a, b, op, ty);
                    self.stack.push(res);
                }
                Opcode::IntegerUnary { op, ty } => {
                    let a = self.stack.pop().unwrap();
                    let res = integer_unary_execute(a, op, ty);
                    self.stack.push(res);
                }
                Opcode::StoreLocal(id) => {
                    let a= self.stack.pop().unwrap();
                    self.locals[id] = a;
                }
                Opcode::LoadLocal(id) => {
                    let a = self.locals[id];
                    self.stack.push(a);
                }
                Opcode::PushConst(a) => {
                    self.stack.push(a);
                }
                Opcode::Pop => {
                    self.stack.pop().unwrap();
                }
                Opcode::Dup => {
                    let a = *self.stack.last().unwrap();
                    self.stack.push(a);
                }
                Opcode::Jump(diff) => {
                    self.pc = self.pc.offset_by(diff);
                }
                Opcode::BranchFalse(diff) => {
                    let a = self.stack.pop().unwrap();
                    if unsafe { a.u64 == 0 } {
                        self.pc = self.pc.offset_by(diff);
                        continue;
                    }
                }
                Opcode::BranchTrue(diff) => {
                    let a = self.stack.pop().unwrap();
                    if unsafe { a.u64 != 0 } {
                        self.pc = self.pc.offset_by(diff);
                        continue;
                    }
                }
                Opcode::Return => {
                    break;
                }
            }

            self.pc = self.pc.offset_by(JumpOffset(1));
        }
    }
}
