use num_traits::PrimInt;

#[derive(Clone, Copy)]
pub enum Opcode {
    IntegerBinary {
        op: IntegerBinaryOp,
        ty: PrimitiveType,
    },
    LoadConst(u32),
    LoadLocal(u32),
    StoreLocal(u32),
}

#[derive(Clone, Copy)]
pub enum IntegerBinaryOp {
    // Compare op
    Eq,
    NEq,
    Lt,
    LtEq,
    Gt,
    GtEq,

    // Add op
    Add,
    Sub,
    Or,
    Xor,

    // Mul op
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    And,
    AndNot,
}

impl IntegerBinaryOp {
    fn execute<T: PrimInt>(self, a: T, b: T) -> T {
        use self::IntegerBinaryOp::*;
        match self {
            Eq =>   T::from((a == b) as u8).unwrap(),
            NEq =>  T::from((a != b) as u8).unwrap(),
            Lt =>   T::from((a <  b) as u8).unwrap(),
            LtEq => T::from((a <= b) as u8).unwrap(),
            Gt =>   T::from((a >  b) as u8).unwrap(),
            GtEq => T::from((a >= b) as u8).unwrap(),
            Add => a + b,
            Sub => a - b,
            Or => a | b,
            Xor => a ^ b,
            Mul => a * b,
            Div => a / b,
            Rem => a % b,
            Shl => a << b.to_usize().unwrap(),
            Shr => a >> b.to_usize().unwrap(),
            And => a & b,
            AndNot => a & !b,
        }
    }
}

#[derive(Clone, Copy)]
enum IntegerUnaryOp {
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

    Ptr,
}

#[derive(Clone, Copy)]
pub union Primitive {
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,

    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,

    ptr: usize,
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

                Ptr => Primitive { ptr: read(ptr as *const usize) },
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

                Ptr => write(ptr as *mut usize, self.ptr),
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

            Ptr => Primitive { ptr: op.execute(a.ptr, b.ptr) },
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

            Ptr => Primitive { ptr: op.execute(a.ptr) },
        }
    }
}
