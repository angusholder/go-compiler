use fnv::FnvHashMap;

use ast;
use utils::result::{ Span, CompileResult };
use vm::Primitive;
use utils::intern::Atom;
use types::{ self, Type, TypeRegistry, TypeRef };

pub enum DeclarationKind {
    Const {
        ty: TypeRef,
        val: Primitive,
    },
    Var(TypeRef),
    Type(TypeRef),
    // TODO: Package(PackageRef), Function
}

pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

struct Scope {
    decls: FnvHashMap<Atom, Declaration>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            decls: FnvHashMap::default(),
        }
    }
}

pub struct Environment {
    scopes: Vec<Scope>,
    type_registry: TypeRegistry,
}

impl Environment {
    pub fn new() -> Environment {
        let mut universe_scope = Scope::new();
        let mut type_registry = TypeRegistry::new();
        for &(name, ty_ref, ref ty) in types::BUILTINS {
            type_registry.set(ty_ref, ty.clone());

            // Unnamed types like `Unresolved` and `LiteralInt` have no name, so don't give
            // them a declaration
            if name != "" {
                universe_scope.decls.insert(Atom::from(name), Declaration {
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
            if let Some(decl) = scope.decls.get(&name) {
                return Some(decl);
            }
        }
        None
    }

    pub fn insert_decl(&mut self, name: Atom, decl: Declaration) {
        let scope = self.scopes.first_mut().unwrap();
        scope.decls.insert(name, decl);
    }

    pub fn get_type(&self, type_ref: TypeRef) -> &Type {
        self.type_registry.lookup(type_ref)
    }

    pub fn insert_type(&mut self, ty: Type) -> TypeRef {
        self.type_registry.insert(ty)
    }
}

pub fn type_check(expr: &mut ast::Expr, env: &Environment) -> CompileResult<TypeRef> {
    use self::ast::ExprKind::*;
    match expr.kind {
        Binary { op, ref mut left, ref mut right } => {
            use ast::BinaryOp::*;

            type_check(left, env)?;
            type_check(right, env)?;

            match op {
                LogOr | LogAnd => {

                }

                Equals | NotEqual | Less | LessOrEqual | Greater | GreaterOrEqual => {

                }

                Add | Sub | Or | Xor | Mul | Div | Remainder | And | AndNot => {

                }

                LShift | RShift => {

                }
            }
            unimplemented!()
        }
        Literal(ast::Literal::Int(_)) => {
            Ok(types::LITERAL_INT)
        }
        Literal(ast::Literal::Ident(ident)) => {
            match env.get_decl(ident).map(|d| &d.kind) {
                Some(&DeclarationKind::Var(ty)) | Some(&DeclarationKind::Const { ty, .. }) => {
                    unimplemented!()
                }
                Some(&DeclarationKind::Type(ty)) => {
                    err!(expr.span, "type {:?} is not an expression", ty)
                }
                None => {
                    err!(expr.span, "unknown identifier {:?}", ident)
                }
            }
        }
        _ => unimplemented!()
    }
}