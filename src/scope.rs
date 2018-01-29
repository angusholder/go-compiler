use fnv::FnvHashMap;

use utils::result::Span;
use vm::Primitive;
use utils::intern::Atom;
use types::{ Type, TypeRegistry, TypeId, UntypedConstant };
use compiler::LocalId;

pub enum DeclarationKind {
    Const {
        ty: TypeId,
        val: Primitive,
    },
    Var(VarDecl),
    Type(TypeId),
    // TODO: Package(PackageRef), Function
}

pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

pub struct VarDecl {
    pub ty: TypeId,
    pub id: LocalId,
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
        use types::builtins::*;
        use types::ARCH;

        let mut universe_scope = Scope::new();
        let mut type_registry = TypeRegistry::new();

        let builtins: &[(&str, TypeId, Type)] = &[
            ("uint8",   UINT8,   Type::Primitive { primitive: U8, name: Atom::from("uint8") }),
            ("uint16",  UINT16,  Type::Primitive { primitive: U16, name: Atom::from("uint16") }),
            ("uint32",  UINT32,  Type::Primitive { primitive: U32, name: Atom::from("uint32") }),
            ("uint64",  UINT64,  Type::Primitive { primitive: U64, name: Atom::from("uint64") }),
            ("int8",    INT8,    Type::Primitive { primitive: I8, name: Atom::from("int8") }),
            ("int16",   INT16,   Type::Primitive { primitive: I16, name: Atom::from("int16") }),
            ("int32",   INT32,   Type::Primitive { primitive: I32, name: Atom::from("int32") }),
            ("int64",   INT64,   Type::Primitive { primitive: I64, name: Atom::from("int64") }),
            ("int",     INT,     Type::Primitive { primitive: ARCH.int, name: Atom::from("int") }),
            ("uint",    UINT,    Type::Primitive { primitive: ARCH.uint, name: Atom::from("uint") }),
            ("uintptr", UINTPTR, Type::Primitive { primitive: ARCH.uintptr, name: Atom::from("uintptr") }),
            ("bool",    BOOL,    Type::Primitive { primitive: U8, name: Atom::from("bool") }),
            ("rune",    RUNE,    Type::Alias { origin: INT32 }),
            ("byte",    BYTE,    Type::Alias { origin: UINT8 }),

            ("", UNTYPED_INT,    Type::Untyped { ty: UntypedConstant::Int, name: Atom::from("untyped int") }),
            ("", UNTYPED_BOOL,   Type::Untyped { ty: UntypedConstant::Bool, name: Atom::from("untyped bool") }),
        ];

        for &(name, ty_ref, ref ty) in builtins {
            type_registry.set(ty_ref, ty.clone());

            // Untyped constants aren't nameable, so don't give them a declaration
            if name != "" {
                universe_scope.decl_from_atom.insert(Atom::from(name), Declaration {
                    kind: DeclarationKind::Type(ty_ref),
                    span: Span::INVALID,
                });
            }
        }

        universe_scope.decl_from_atom.insert(Atom::from("true"), Declaration {
            kind: DeclarationKind::Const { ty: UNTYPED_BOOL, val: Primitive { u8: 1 } },
            span: Span::INVALID,
        });

        universe_scope.decl_from_atom.insert(Atom::from("false"), Declaration {
            kind: DeclarationKind::Const { ty: UNTYPED_BOOL, val: Primitive { u8: 0 } },
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

    pub fn get_decl(&self, name: Atom) -> Option<&Declaration> {
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

    pub fn get_local_var_decl(&self, name: Atom) -> Option<&VarDecl> {
        let local_scope = self.scopes.last().unwrap();
        let local = local_scope.decl_from_atom.get(&name);
        if let Some(&Declaration { kind: DeclarationKind::Var(ref decl), .. }) = local {
            Some(decl)
        } else {
            None
        }
    }

    pub fn get_type(&self, type_ref: TypeId) -> &Type {
        self.type_registry.lookup(type_ref)
    }

    pub fn insert_type(&mut self, ty: Type) -> TypeId {
        self.type_registry.insert(ty)
    }
}
