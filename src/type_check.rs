use std::collections::HashMap;

use num_traits::PrimInt;

use ast;
use utils::result::{ Span, CompileResult };
use vm::Primitive;

#[derive(Clone, Copy, Debug)]
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

    Bool,
}

impl PrimitiveType {
    fn is_unsigned_int(&self) -> bool {
        use self::PrimitiveType::*;
        match *self {
            U8 | U16 | U32 | U64 | Uint => true,
            I8 | I16 | I32 | I64 | Int => false,
            Bool => false,
        }
    }

    fn is_signed_int(&self) -> bool {
        use self::PrimitiveType::*;
        match *self {
            U8 | U16 | U32 | U64 | Uint => false,
            I8 | I16 | I32 | I64 | Int => true,
            Bool => false,
        }
    }
}

pub enum DeclarationKind {
    Const {
        ty: Type,
        val: Primitive,
    },
    Var(Type),
    Type(PrimitiveType),
    // TODO: Function
}

pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

struct Scope {
    decls: HashMap<String, Declaration>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            decls: HashMap::new(),
        }
    }

    fn make_universe() -> Scope {
        Scope::new()
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

    pub fn get_decl(&self, name: &str) -> Option<&Declaration> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.decls.get(name) {
                return Some(decl);
            }
        }
        None
    }

    pub fn insert_decl(&mut self, name: &str, decl: Declaration) {
        let scope = self.scopes.first_mut().unwrap();
        scope.decls.insert(name.to_string(), decl);
    }
}

#[derive(Debug)]
pub enum Type {
    Unresolved,
    LiteralInt,
    Primitive(PrimitiveType),
    // TODO: Alias, Compound
}

pub fn type_check(expr: &mut ast::Expr, env: &Environment) -> CompileResult<Type> {
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
            Ok(Type::LiteralInt)
        }
        Literal(ast::Literal::Ident(ref ident)) => {
            match env.get_decl(ident).map(|d| &d.kind) {
                Some(&DeclarationKind::Var(ref ty)) | Some(&DeclarationKind::Const { ref ty, .. }) => {
                    unimplemented!()
                }
                Some(&DeclarationKind::Type(ref ty)) => {
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