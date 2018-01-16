use utils::id::IdVecMap;
use vm::PrimitiveType;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct TypeId(u32);
impl_id!(TypeId);

pub const UNRESOLVED: TypeId = TypeId(0);
pub const LITERAL_INT: TypeId = TypeId(19);

pub const UINT8: TypeId = TypeId(1);
pub const UINT16: TypeId = TypeId(2);
pub const UINT32: TypeId = TypeId(3);
pub const UINT64: TypeId = TypeId(4);
pub const INT8: TypeId = TypeId(5);
pub const INT16: TypeId = TypeId(6);
pub const INT32: TypeId = TypeId(7);
pub const INT64: TypeId = TypeId(8);

//pub const FLOAT32:    TypeRef = TypeRef(9);
//pub const FLOAT64:    TypeRef = TypeRef(10);
//pub const COMPLEX64:  TypeRef = TypeRef(11);
//pub const COMPLEX128: TypeRef = TypeRef(12);

pub const BOOL: TypeId = TypeId(13);
pub const BYTE: TypeId = TypeId(14);
pub const RUNE: TypeId = TypeId(15);

pub const INT: TypeId = TypeId(16);
pub const UINT: TypeId = TypeId(17);
pub const UINTPTR: TypeId = TypeId(18);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Unresolved,
    LiteralInt,
    Primitive(PrimitiveType),
    Alias(TypeId),
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

pub static BUILTINS: &[(&str, TypeId, Type)] = &[
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
    types: IdVecMap<TypeId, Type>,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        TypeRegistry {
            types: IdVecMap::new(),
        }
    }

    pub fn lookup(&self, type_ref: TypeId) -> &Type {
        &self.types[type_ref]
    }

    pub fn set(&mut self, type_ref: TypeId, ty: Type) {
        self.types.insert(type_ref, ty);
    }

    pub fn insert(&mut self, ty: Type) -> TypeId {
        let type_ref = self.next_type_ref();
        self.types.insert(type_ref, ty);
        type_ref
    }

    fn next_type_ref(&self) -> TypeId {
        self.types.next_key()
    }
}

