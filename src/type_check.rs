use fnv::FnvHashMap;

use ast;
use utils::result::{ Span, CompileResult };
use vm::Primitive;
use utils::intern::Atom;
use utils::id::IdVecMap;
use types::{ self, Type, TypeRegistry, TypeId, UntypedConstant };
use ast::ExprId;

pub enum DeclarationKind {
    Const {
        ty: TypeId,
        val: Primitive,
    },
    Var(TypeId),
    Type(TypeId),
    // TODO: Package(PackageRef), Function
}

pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
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
        use types::Type::*;
        use types::builtins::*;
        use types::ARCH;

        let mut universe_scope = Scope::new();
        let mut type_registry = TypeRegistry::new();

        let builtins: &[(&str, TypeId, Type)] = &[
            ("uint8",   UINT8,   Primitive { primitive: U8, name: Atom::from("uint8") }),
            ("uint16",  UINT16,  Primitive { primitive: U16, name: Atom::from("uint16") }),
            ("uint32",  UINT32,  Primitive { primitive: U32, name: Atom::from("uint32") }),
            ("uint64",  UINT64,  Primitive { primitive: U64, name: Atom::from("uint64") }),
            ("int8",    INT8,    Primitive { primitive: I8, name: Atom::from("int8") }),
            ("int16",   INT16,   Primitive { primitive: I16, name: Atom::from("int16") }),
            ("int32",   INT32,   Primitive { primitive: I32, name: Atom::from("int32") }),
            ("int64",   INT64,   Primitive { primitive: I64, name: Atom::from("int64") }),
            ("int",     INT,     Primitive { primitive: ARCH.int, name: Atom::from("int") }),
            ("uint",    UINT,    Primitive { primitive: ARCH.uint, name: Atom::from("uint") }),
            ("uintptr", UINTPTR, Primitive { primitive: ARCH.uintptr, name: Atom::from("uintptr") }),
            ("bool",    BOOL,    Primitive { primitive: U8, name: Atom::from("bool") }),
            ("rune",    RUNE,    Alias { origin: INT32 }),
            ("byte",    BYTE,    Alias { origin: UINT8 }),

            ("", UNTYPED_INT, Untyped { ty: UntypedConstant::Int, name: Atom::from("untyped int") }),
            ("", UNTYPED_BOOL, Untyped { ty: UntypedConstant::Bool, name: Atom::from("untyped bool") }),
        ];

        for &(name, ty_ref, ref ty) in builtins {
            type_registry.set(ty_ref, ty.clone());

            // Unnamed types like `Unresolved` and `LiteralInt` have no name, so don't give
            // them a declaration
            if name != "" {
                universe_scope.decl_from_atom.insert(Atom::from(name), Declaration {
                    kind: DeclarationKind::Type(ty_ref),
                    span: Span::INVALID,
                });
            }
        }

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

    pub fn get_type(&self, type_ref: TypeId) -> &Type {
        self.type_registry.lookup(type_ref)
    }

    pub fn insert_type(&mut self, ty: Type) -> TypeId {
        self.type_registry.insert(ty)
    }
}

struct TypeChecker {
    typeid_from_exprid: IdVecMap<ExprId, TypeId>,
}

fn type_check_binary_op(env: &Environment,
                        op: ast::BinaryOp,
                        left_ty: TypeId,
                        right_ty: TypeId,
                        span: Span) -> CompileResult<TypeId> {
    use self::ast::BinaryOp::*;
    match op {
        LogOr | LogAnd => {
            if !left_ty.is_boolean(env) || !right_ty.is_boolean(env) {
                return err!(span, "{} operator expects boolean operands, got {} and {}",
                                op, left_ty.name(env), right_ty.name(env));
            }

            if !left_ty.equal_identity(right_ty, env) {
                return err!(span, "invalid operation {}, mismatched types {} and {}",
                                op, left_ty.name(env), right_ty.name(env));
            }

            Ok(left_ty)
        }

        Equals | NotEqual | Less | LessOrEqual | Greater | GreaterOrEqual => {
            if !left_ty.equal_identity(right_ty, env) {
                return err!(span, "invalid operation {}, mismatched types {} and {}",
                                op, left_ty.name(env), right_ty.name(env));
            }

            Ok(types::UNTYPED_BOOL)
        }

        Add | Sub | Or | Xor | Mul | Div | Remainder | And | AndNot => {
            if !left_ty.equal_identity(right_ty, env) {
                return err!(span, "invalid operation {}, mismatched types {} and {}",
                                op, left_ty.name(env), right_ty.name(env));
            }

            if !left_ty.is_integer(env) {
                return err!(span, "invalid operation {}, expected integer type, got {} and {}",
                                op, left_ty.name(env), right_ty.name(env));
            }

            Ok(left_ty)
        }

        LShift | RShift => {
            if !right_ty.is_unsigned_integer(env) {
                return err!(span, "invalid operation, shift count type {} must be unsigned integer",
                                right_ty.name(env));
            }

            if left_ty.is_integer(env) {
                return err!(span, "shift of type {}, must be integer", left_ty.name(env));
            }

            Ok(left_ty)
        }
    }
}

impl TypeChecker {
    pub fn check_expr(&mut self, expr: &ast::Expr, env: &Environment) -> CompileResult<TypeId> {
        use self::ast::ExprKind::*;
        let result_type = match expr.kind {
            Binary { op, ref left, ref right } => {
                let left_ty = self.check_expr(left, env)?;
                let right_ty = self.check_expr(right, env)?;

                type_check_binary_op(env, op, left_ty, right_ty, expr.span)?
            }
            Literal(ast::Literal::Int(_)) => {
                types::UNTYPED_INT
            }
            Literal(ast::Literal::Ident(ident)) => {
                match env.get_decl(ident).map(|d| &d.kind) {
                    Some(&DeclarationKind::Var(ty)) | Some(&DeclarationKind::Const { ty, .. }) => {
                        ty
                    }
                    Some(&DeclarationKind::Type(ty)) => {
                        return err!(expr.span, "type {} is not an expression", ty.name(env));
                    }
                    None => {
                        return err!(expr.span, "unknown identifier {}", ident);
                    }
                }
            }
            _ => unimplemented!()
        };

        self.typeid_from_exprid.insert(expr.id, result_type);
        Ok(result_type)
    }
}