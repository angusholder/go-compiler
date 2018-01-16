use fnv::FnvHashMap;

use ast;
use utils::result::{ Span, CompileResult };
use vm::Primitive;
use utils::intern::Atom;
use utils::id::IdVecMap;
use types::{self, Type, TypeRegistry, TypeId};
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

    pub fn get_type(&self, type_ref: TypeId) -> &Type {
        self.type_registry.lookup(type_ref)
    }

    pub fn insert_type(&mut self, ty: Type) -> TypeId {
        self.type_registry.insert(ty)
    }
}

struct TypeChecker {
    types: IdVecMap<ExprId, TypeId>,
}

impl TypeChecker {
    pub fn check_expr(&mut self, expr: &ast::Expr, env: &Environment) -> CompileResult<TypeId> {
        use self::ast::ExprKind::*;
        match expr.kind {
            Binary { op, ref left, ref right } => {
                use ast::BinaryOp::*;

                let left_ty = self.check_expr(left, env)?;
                let right_ty = self.check_expr(right, env)?;

                let result_type = match op {
                    LogOr | LogAnd => {
                        if left_ty != types::BOOL || right_ty != types::BOOL {
                            return err!(expr.span, "{} operator expects boolean operands, got {:?} and {:?}",
                                op, left_ty, right_ty);
                        }
                        types::BOOL
                    }

                    Equals | NotEqual | Less | LessOrEqual | Greater | GreaterOrEqual => {
                        if left_ty != right_ty {
                            return err!(expr.span, "{} operator not defined for {:?} and {:?}",
                                op, left_ty, right_ty);
                        }
                        types::BOOL
                    }

                    Add | Sub | Or | Xor | Mul | Div | Remainder | And | AndNot => {
                        if left_ty != right_ty {
                            return err!(expr.span, "{} operator not defined for {:?} and {:?}",
                                op, left_ty, right_ty);
                        }
                        left_ty
                    }

                    LShift | RShift => {
                        unimplemented!()
                    }
                };

                self.types.insert(expr.id, result_type);

                Ok(result_type)
            }
            Literal(ast::Literal::Int(_)) => {
                Ok(types::LITERAL_INT)
            }
            Literal(ast::Literal::Ident(ident)) => {
                match env.get_decl(ident).map(|d| &d.kind) {
                    Some(&DeclarationKind::Var(ty)) | Some(&DeclarationKind::Const { ty, .. }) => {
                        Ok(ty)
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
}