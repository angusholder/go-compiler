use std::collections::HashMap;

use num_traits::PrimInt;

pub enum PrimitiveType {
    U8,
    U16,
    U32,
    U64,

    I8,
    I16,
    I32,
    I64,

    Int,
    Uint,
}

pub type TypeRef = u32;

pub enum Declaration {
    Const {
        ty: TypeRef,
        val: Primitive,
    },
    Var(TypeRef),
    Type(PrimitiveType),
    Function {

    },
}

pub struct Scope {
    pub names: HashMap<String, TypeRef>,
    pub decls: Vec<Declaration>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            names: HashMap::new(),
            decls: Vec::new(),
        }
    }

    pub fn make_universe() -> Scope {
        use self::PrimitiveType::*;
        const TYPES: &[(&str, PrimitiveType)] = &[
            ("uint8", U8),
            ("uint16", U16),
            ("uint32", U32),
            ("uint64", U64),

            ("int8", I8),
            ("int16", I16),
            ("int32", I32),
            ("int64", I64),

            ("int", Int),
            ("uint", Uint),

            ("rune", I32),
            ("byte", U8),
        ];

        let mut scope = Scope::new();

        for (name, ty) in TYPES {
            scope.decls.insert(name.to_string(), Declaration::Type(ty));
        }

        scope
    }
}

pub struct Environment {
    scopes: Vec<Scope>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scopes: vec![Scope::make_universe()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub fn get_type(&self, name: &str) -> Option<&Declaration> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.decls.get(name) {
                return Some(decl);
            }
        }
        None
    }

    pub fn insert_type(&mut self, name: &str, decl: Declaration) {
        let scope = self.scopes.first_mut().unwrap();
        scope.decls.insert(name.to_string(), decl);
    }
}

pub fn type_check(src: &mut SourceFile) {

}

pub enum ResolvedType {
    Primitive(PrimitiveType),
    Alias(TypeIndex),
    Compound(CompoundType),
}

pub enum Opcode {
    IntegerBinary {
        op: IntegerBinaryOp,
    },

}

#[derive(Clone, Copy)]
pub enum IntegerBinaryOp {
    // Compare op
    Equals,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,

    // Add op
    Add,
    Sub,
    Or,
    Xor,

    // Mul op
    Mul,
    Div,
    Modulo,
    LShift,
    RShift,
    And,
    AndNot,
}

impl IntegerBinaryOp {
    fn execute<T: PrimInt>(self, a: T, b: T) -> T {
        use self::IntegerBinaryOp::*;
        match self {
            Equals => a == b,
            NotEqual => a != b,
            Less => a < b,
            LessOrEqual => a <= b,
            Greater => a > b,
            GreaterOrEqual => a >= b,
            Add => a + b,
            Sub => a - b,
            Or => a | b,
            Xor => a ^ b,
            Mul => a * b,
            Div => a / b,
            Modulo => a % b,
            LShift => a << b,
            RShift => a >> b,
            And => a & b,
            AndNot => a & !b,
        }
    }
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

    int: isize,
    uint: usize,
}

fn integer_execute(a: Primitive, b: Primitive, op: IntegerBinaryOp, ty: PrimitiveType) -> Primitive {
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

            Int  => Primitive { int:  op.execute(a.int,  b.int) },
            Uint => Primitive { uint: op.execute(a.uint, b.uint) },
        }
    }
}

pub struct Function {
    code: Vec<Opcode>,
    consts: Vec<Primitive>,
}
