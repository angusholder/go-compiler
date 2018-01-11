use vec_map::VecMap;

use vm::PrimitiveType;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TypeRef(u32);

pub const UNRESOLVED: TypeRef = TypeRef(0);
pub const LITERAL_INT: TypeRef = TypeRef(19);

pub const UINT8:    TypeRef = TypeRef(1);
pub const UINT16:   TypeRef = TypeRef(2);
pub const UINT32:   TypeRef = TypeRef(3);
pub const UINT64:   TypeRef = TypeRef(4);
pub const INT8:     TypeRef = TypeRef(5);
pub const INT16:    TypeRef = TypeRef(6);
pub const INT32:    TypeRef = TypeRef(7);
pub const INT64:    TypeRef = TypeRef(8);

//pub const FLOAT32:    TypeRef = TypeRef(9);
//pub const FLOAT64:    TypeRef = TypeRef(10);
//pub const COMPLEX64:  TypeRef = TypeRef(11);
//pub const COMPLEX128: TypeRef = TypeRef(12);

pub const BOOL:     TypeRef = TypeRef(13);
pub const BYTE:     TypeRef = TypeRef(14);
pub const RUNE:     TypeRef = TypeRef(15);

pub const INT:      TypeRef = TypeRef(16);
pub const UINT:     TypeRef = TypeRef(17);
pub const UINTPTR:  TypeRef = TypeRef(18);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Unresolved,
    LiteralInt,
    Primitive(PrimitiveType),
    Alias(TypeRef),
}

pub struct ArchitectureTypes {
    uint: PrimitiveType,
    int: PrimitiveType,
    uintptr: PrimitiveType,
}

use self::PrimitiveType::*;
use self::Type::*;

#[cfg(target_arch = "x86_64")]
const ARCH: ArchitectureTypes = ArchitectureTypes {
    uint: U64,
    int: I64,
    uintptr: U64,
};
#[cfg(target_arch = "x86")]
const ARCH: ArchitectureTypes = ArchitectureTypes {
    uint: U32,
    int: I32,
    uintptr: U32,
};

pub static BUILTINS: &[(&str, TypeRef, Type)] = &[
    ("",        UNRESOLVED, Unresolved),
    ("",        LITERAL_INT, LiteralInt),

    ("uint8",   UINT8,   Primitive(U8)),
    ("uint16",  UINT16,  Primitive(U16)),
    ("uint32",  UINT32,  Primitive(U32)),
    ("uint64",  UINT64,  Primitive(U64)),
    ("int8",    INT8,    Primitive(I8)),
    ("int16",   INT16,   Primitive(I16)),
    ("int32",   INT32,   Primitive(I32)),
    ("int64",   INT64,   Primitive(I64)),
    ("int",     INT,     Primitive(ARCH.int)),
    ("uint",    UINT,    Primitive(ARCH.uint)),
    ("uintptr", UINTPTR, Primitive(ARCH.uintptr)),
    ("bool",    BOOL,    Primitive(U8)),
    ("rune",    RUNE,    Alias(INT32)),
    ("byte",    BYTE,    Alias(UINT8)),
];

pub struct TypeRegistry {
    types: VecMap<Type>,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        TypeRegistry {
            types: VecMap::new(),
        }
    }

    pub fn lookup(&self, type_ref: TypeRef) -> &Type {
        &self.types[type_ref.0 as usize]
    }

    pub fn set(&mut self, type_ref: TypeRef, ty: Type) {
        self.types.insert(type_ref.0 as usize, ty);
    }

    pub fn insert(&mut self, ty: Type) -> TypeRef {
        let type_ref = self.next_type_ref();
        self.types.insert(type_ref.0 as usize, ty);
        type_ref
    }

    fn next_type_ref(&self) -> TypeRef {
        TypeRef(self.types.len() as u32)
    }
}

