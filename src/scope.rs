use fnv::FnvHashMap;

use utils::result::Span;
use vm::Primitive;
use utils::intern::Atom;
use types::{ Type, TypeRegistry, TypeId, UntypedConstant };
use compiler::LocalId;
use compiler::CodeOffset;

pub enum Declaration {
    Const {
        span: Span,
        ty: TypeId,
        val: Primitive,
    },
    Var {
        span: Span,
        ty: TypeId,
        id: LocalId,
    },
    Type {
        span: Span,
        ty: TypeId,
    },
    // TODO: Package(PackageRef), Function
}

impl Declaration {
    pub fn span(&self) -> Span {
        match *self {
            Declaration::Const { span, .. } |
            Declaration::Var { span, .. } |
            Declaration::Type { span, .. } => span
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            Declaration::Var { .. } => true,
            Declaration::Const { .. } |
            Declaration::Type { .. } => false,
        }
    }
}

struct Scope {
    decl_from_atom: FnvHashMap<Atom, Declaration>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            decl_from_atom: FnvHashMap::default(),
        }
    }
}

pub struct Environment {
    scopes: Vec<Scope>,
    type_registry: TypeRegistry,
}

impl Environment {
    pub fn new() -> Environment {
        use vm::PrimitiveType::*;
        use vm::PrimitiveType;
        use types::builtins::*;
        use types::ARCH;

        let mut universe_scope = Scope::new();
        let mut type_registry = TypeRegistry::new();

        fn primitive(primitive: PrimitiveType, name: Atom) -> Type {
            Type::Primitive { primitive, name }
        }

        let builtins: &[(&str, TypeId, Type)] = &[
            ("uint8",   UINT8,   primitive(U8, Atom("uint8"))),
            ("uint16",  UINT16,  primitive(U16, Atom("uint16"))),
            ("uint32",  UINT32,  primitive(U32, Atom("uint32"))),
            ("uint64",  UINT64,  primitive(U64, Atom("uint64"))),
            ("int8",    INT8,    primitive(I8, Atom("int8"))),
            ("int16",   INT16,   primitive(I16, Atom("int16"))),
            ("int32",   INT32,   primitive(I32, Atom("int32"))),
            ("int64",   INT64,   primitive(I64, Atom("int64"))),
            ("int",     INT,     primitive(ARCH.int, Atom("int"))),
            ("uint",    UINT,    primitive(ARCH.uint, Atom("uint"))),
            ("uintptr", UINTPTR, primitive(ARCH.uintptr, Atom("uintptr"))),
            ("bool",    BOOL,    primitive(U8, Atom("bool"))),
            ("rune",    RUNE,    primitive(I32, Atom("rune"))),
            ("byte",    BYTE,    primitive(U8, Atom("byte"))),
        ];

        for &(name, ty_ref, ref ty) in builtins {
            type_registry.set(ty_ref, ty.clone());

            universe_scope.decl_from_atom.insert(Atom(name), Declaration::Type {
                ty: ty_ref,
                span: Span::INVALID,
            });
        }

        type_registry.set(UNTYPED_INT, Type::Untyped {
            ty: UntypedConstant::Int,
            name: Atom("untyped int")
        });

        type_registry.set(UNTYPED_BOOL, Type::Untyped {
            ty: UntypedConstant::Bool,
            name: Atom("untyped bool")
        });

        universe_scope.decl_from_atom.insert(Atom("true"), Declaration::Const {
            ty: UNTYPED_BOOL,
            val: Primitive { u8: 1 },
            span: Span::INVALID,
        });

        universe_scope.decl_from_atom.insert(Atom("false"), Declaration::Const {
            ty: UNTYPED_BOOL,
            val: Primitive { u8: 0 },
            span: Span::INVALID,
        });

        let package_scope = Scope::new();

        Environment {
            scopes: vec![universe_scope, package_scope],
            type_registry,
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() < 2 {
            panic!("You must not pop the universe scope");
        }

        self.scopes.pop().unwrap();
    }

    pub fn decl(&self, name: Atom) -> Option<&Declaration> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.decl_from_atom.get(&name) {
                return Some(decl);
            }
        }
        None
    }

    pub fn insert_decl(&mut self, name: Atom, decl: Declaration) {
        let scope = self.scopes.first_mut().unwrap();
        scope.decl_from_atom.insert(name, decl);
    }

    pub fn local_decl(&self, name: Atom) -> Option<&Declaration> {
        let local_scope = self.scopes.last().unwrap();
        local_scope.decl_from_atom.get(&name)
    }

    pub fn get_type(&self, type_ref: TypeId) -> &Type {
        self.type_registry.lookup(type_ref)
    }

    pub fn insert_type(&mut self, ty: Type) -> TypeId {
        self.type_registry.insert(ty)
    }
}
