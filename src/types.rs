use utils::id::IdVecMap;
use vm::PrimitiveType;
use utils::intern::Atom;
use scope::Environment;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct TypeId(u32);
impl_id!(TypeId);

pub use self::builtins::*;
pub mod builtins {
    use super::TypeId;

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

    pub const UNTYPED_BOOL: TypeId = TypeId(19);
    pub const UNTYPED_INT: TypeId = TypeId(20);
}

impl TypeId {
    pub fn name(self, env: &Environment) -> Atom {
        match *env.get_type(self) {
            Type::Primitive { name, .. } => name,
            Type::Alias { origin, .. } => origin.name(env),
            Type::DistinctAlias { name, .. } => name,
            Type::Untyped { name, .. } => name,
        }
    }

    /// Return the original type, drilling down through distinct type declarations and type aliases
    /// For example, given the following declarations
    /// ```
    /// // Type alias of bool
    /// type Foo = bool
    /// // Distinct type alias of bool
    /// type Bar bool
    /// // Distinct type alias of the non-distinct alias Foo, aka a distinct alias of `bool`
    /// type Baz Foo
    /// ```
    /// all return `bool` as their underlying type.
    ///
    /// As a usage example, remember that if statements only accept a boolean type as their
    /// condition, but we want to allow distinct and non-distinct type aliases as valid types,
    /// so this method can be used to determine that.
    pub fn underlying_type(self, env: &Environment) -> TypeId {
        match *env.get_type(self) {
            Type::Primitive { .. } => self,
            Type::Alias { origin, .. } => origin.underlying_type(env),
            Type::DistinctAlias { origin, .. } => origin.underlying_type(env),
            Type::Untyped { ty, .. } => ty.as_type_id(),
        }
    }

    pub fn as_primitive_type(self, env: &Environment) -> PrimitiveType {
        match *env.get_type(self) {
            Type::Primitive { primitive, .. } => primitive,
            Type::Alias { origin, .. } => origin.as_primitive_type(env),
            Type::DistinctAlias { origin, .. } => origin.as_primitive_type(env),
            Type::Untyped { ty, .. } => ty.as_type_id().as_primitive_type(env),
        }
    }

    pub fn resolve_aliases(self, env: &Environment) -> TypeId {
        if let Type::Alias { origin, .. } = *env.get_type(self) {
            origin.resolve_aliases(env)
        } else {
            self
        }
    }

    pub fn equal_identity(self, other: TypeId, env: &Environment) -> bool {
        self.resolve_aliases(env) == other.resolve_aliases(env)
    }

    pub fn is_integer(self, env: &Environment) -> bool {
        match self.underlying_type(env) {
            UINT8 | UINT16 | UINT32 | UINT64 |
            INT8 | INT16 | INT32 | INT64 |
            INT | UINT | UINTPTR => true,
            _ => false,
        }
    }

    pub fn is_signed_integer(self, env: &Environment) -> bool {
        match self.underlying_type(env) {
            INT8 | INT16 | INT32 | INT64 | INT => true,
            _ => false,
        }
    }

    pub fn is_unsigned_integer(self, env: &Environment) -> bool {
        match self.underlying_type(env) {
            UINT8 | UINT16 | UINT32 | UINT64 | UINT | UINTPTR => true,
            _ => false,
        }
    }

    pub fn is_boolean(self, env: &Environment) -> bool {
        self.underlying_type(env) == BOOL
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Primitive {
        primitive: PrimitiveType,
        name: Atom,
    },
    Alias {
        origin: TypeId,
    },
    DistinctAlias {
        origin: TypeId,
        name: Atom,
    },
    Untyped {
        ty: UntypedConstant,
        name: Atom,
    }
}

/// See:
/// https://golang.org/ref/spec#Constants
/// https://golang.org/ref/spec#Constant_expressions
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UntypedConstant {
    Bool,
    Int,
    Rune,
    // TODO: Float, Complex, String
}

impl UntypedConstant {
    pub fn as_type_id(&self) -> TypeId {
        use self::UntypedConstant::*;
        match *self {
            Bool => BOOL,
            Int => INT,
            Rune => RUNE,
        }
    }
}

pub struct ArchitectureTypes {
    pub uint: PrimitiveType,
    pub int: PrimitiveType,
    pub uintptr: PrimitiveType,
}

#[cfg(target_arch = "x86_64")]
pub const ARCH: ArchitectureTypes = ArchitectureTypes {
    uint: PrimitiveType::U64,
    int: PrimitiveType::I64,
    uintptr: PrimitiveType::U64,
};
#[cfg(target_arch = "x86")]
pub const ARCH: ArchitectureTypes = ArchitectureTypes {
    uint: PrimitiveType::U32,
    int: PrimitiveType::I32,
    uintptr: PrimitiveType::U32,
};

pub struct TypeRegistry {
    type_from_id: IdVecMap<TypeId, Type>,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        TypeRegistry {
            type_from_id: IdVecMap::new(),
        }
    }

    pub fn lookup(&self, type_ref: TypeId) -> &Type {
        &self.type_from_id[type_ref]
    }

    pub fn set(&mut self, type_ref: TypeId, ty: Type) {
        self.type_from_id.insert(type_ref, ty);
    }

    pub fn insert(&mut self, ty: Type) -> TypeId {
        let type_ref = self.next_type_ref();
        self.type_from_id.insert(type_ref, ty);
        type_ref
    }

    fn next_type_ref(&self) -> TypeId {
        self.type_from_id.next_key()
    }
}

