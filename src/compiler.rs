use ast;
use types;
use scope::*;
use utils::id::IdVec;
use utils::result::CompileResult;
use vm::{ Opcode, Primitive };
use utils::id::IdVecMap;
use token::AssignOp;
use ast::ExprId;
use types::TypeId;
use utils::result::Span;

pub struct Compiler {
    env: Environment,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            env: Environment::new(),
        }
    }

    pub fn compile(&mut self, source: &ast::SourceFile) -> CompileResult<()> {
        // First step is to ensure that all imports are available for use.
        self.compile_imports(source)?;

        // Top level declarations use block scoping rather than lexical, so do a pass on them just
        // to collect all their names for name resolution in the following passes.
        self.collect_top_level_declarations(source)?;

        // https://golang.org/ref/spec#Package_initialization
        // Generate code that initializes global variables in the specified order, then calls all
        // `init` functions in order of declaration.
        self.compile_top_level_initializers(source)?;

        // Now that all top-level variables are properly initialized, we can compile all the
        // top-level functions.
        self.compile_top_level_functions(source)
    }
}

impl Compiler {
    fn compile_imports(&mut self, source: &ast::SourceFile) -> CompileResult<()> {
        for import in source.imports.iter() {
            unimplemented!()
        }
        Ok(())
    }

    fn collect_top_level_declarations(&mut self, source: &ast::SourceFile) -> CompileResult<()> {
        for decl in source.decls.iter() {
            match *decl {
                ast::TopLevelDecl::Function(ref fn_decl) => {
                    if fn_decl.name.as_str() != "main" {
                        unimplemented!()
                    }
                }
                _ => unimplemented!()
            }
        }
        Ok(())
    }

    fn compile_top_level_initializers(&mut self, source: &ast::SourceFile) -> CompileResult<()> {
        use self::ast::Declaration::*;
        use self::ast::TopLevelDecl::*;
        for decl in source.decls.iter() {
            match *decl {
                Declaration(Var(_)) => {
                    unimplemented!()
                }

                Declaration(Const(_)) => {
                    unimplemented!()
                }

                Function(ref decl) => {
                    if decl.name == "init" {
                        unimplemented!()
                    }
                }

                // Nothing to do
                Method(_) | Declaration(Type(_)) => {}
            }
        }
        Ok(())
    }

    fn compile_top_level_functions(&mut self, source: &ast::SourceFile) -> CompileResult<()> {
        unimplemented!()
    }

    fn compile_top_level_decl(&mut self, decl: &ast::TopLevelDecl) -> CompileResult<()> {
        use self::ast::TopLevelDecl::*;
        match *decl {
            Declaration(ref decl) => {
                self.compile_decl(decl)
            }
            Function(ref func) => {
                self.compile_func(func)?;
                unimplemented!()
            }
            Method(_) => unimplemented!()
        }
    }

    fn compile_decl(&mut self, decl: &ast::Declaration) -> CompileResult<()> {
        unimplemented!()
    }

    fn compile_func(&mut self, func: &ast::FunctionDecl) -> CompileResult<Function> {
        FunctionCompiler::new(&mut self.env, func).compile()
    }
}

pub struct Function {
    code: IdVec<CodeOffset, Opcode>,
    consts: IdVec<ConstId, Primitive>,
}

pub struct FunctionCompiler<'env, 'func> {
    env: &'env mut Environment,
    decl: &'func ast::FunctionDecl,
    code: IdVec<CodeOffset, Opcode>,
    consts: IdVec<ConstId, Primitive>,

    expr_to_type: IdVecMap<ExprId, TypeId>,
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn new(env: &'env mut Environment, decl: &'func ast::FunctionDecl) -> FunctionCompiler<'env, 'func> {
        FunctionCompiler {
            env, decl,
            code: IdVec::new(),
            consts: IdVec::new(),
            expr_to_type: IdVecMap::new(),
        }
    }

    fn compile(mut self) -> CompileResult<Function> {
        assert!(self.decl.sig.params.is_empty());
//        assert!(self.decl.sig.result == ast::FuncResult::None);

        let body: &[ast::Stmt] = self.decl.body.as_ref().unwrap();

        for stmt in body.iter() {
            self.compile_stmt(stmt)?;
        }

        Ok(Function {
            code: self.code,
            consts: self.consts,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct CodeOffset(u32);
impl_id!(CodeOffset);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct LocalId(u32);
impl_id!(LocalId);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ConstId(u32);
impl_id!(ConstId);

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn emit(&mut self, opcode: Opcode) -> CodeOffset {
        self.code.append(opcode)
    }
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn check_binary_op_expr(&mut self,
                            op: ast::BinaryOp,
                            left_ty: TypeId,
                            right_ty: TypeId,
                            span: Span) -> CompileResult<TypeId> {
        use self::ast::BinaryOp::*;
        match op {
            LogOr | LogAnd => {
                if !left_ty.is_boolean(&self.env) || !right_ty.is_boolean(&self.env) {
                    return err!(span, "{} operator expects boolean operands, got {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                if !left_ty.equal_identity(right_ty, &self.env) {
                    return err!(span, "invalid operation {}, mismatched types {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                Ok(left_ty)
            }

            Equals | NotEqual | Less | LessOrEqual | Greater | GreaterOrEqual => {
                if !left_ty.equal_identity(right_ty, &self.env) {
                    return err!(span, "invalid operation {}, mismatched types {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                Ok(types::UNTYPED_BOOL)
            }

            Add | Sub | Or | Xor | Mul | Div | Remainder | And | AndNot => {
                if !left_ty.equal_identity(right_ty, &self.env) {
                    return err!(span, "invalid operation {}, mismatched types {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                if !left_ty.is_integer(&self.env) {
                    return err!(span, "invalid operation {}, expected integer type, got {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                Ok(left_ty)
            }

            LShift | RShift => {
                if !right_ty.is_unsigned_integer(&self.env) {
                    return err!(span, "invalid operation, shift count type {} must be unsigned integer",
                                    right_ty.name(&self.env));
                }

                if left_ty.is_integer(&self.env) {
                    return err!(span, "shift of type {}, must be integer", left_ty.name(&self.env));
                }

                Ok(left_ty)
            }
        }
    }

    pub fn check_expr(&mut self, expr: &ast::Expr) -> CompileResult<TypeId> {
        use self::ast::ExprKind::*;

        if let Some(&ty) = self.expr_to_type.get(expr.id) {
            return Ok(ty);
        }

        let result_type = match expr.kind {
            Binary { op, ref left, ref right } => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;

                self.check_binary_op_expr(op, left_ty, right_ty, expr.span)?
            }
            Literal(ast::Literal::Int(_)) => {
                types::UNTYPED_INT
            }
            Literal(ast::Literal::Ident(ident)) => {
                match self.env.get_decl(ident).map(|d| &d.kind) {
                    Some(&DeclarationKind::Var(VarDecl { ty, .. })) => ty,
                    Some(&DeclarationKind::Const { ty, .. }) => ty,
                    Some(&DeclarationKind::Type(ty)) => {
                        return err!(expr.span, "type {} is not an expression", ty.name(&self.env));
                    }
                    None => {
                        return err!(expr.span, "unknown identifier {}", ident);
                    }
                }
            }
            _ => unimplemented!("{:?}", expr)
        };

        self.expr_to_type.insert(expr.id, result_type);
        Ok(result_type)
    }
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn compile_stmt(&mut self, stmt: &ast::Stmt) -> CompileResult<()> {
        use self::ast::Stmt::*;
        match *stmt {
            If(ref if_stmt) => {
                self.compile_if_stmt(if_stmt)
            }
            Simple(ref simple) => {
                self.compile_simple_stmt(simple)
            }
            _ => unimplemented!()
        }
    }

    fn compile_if_stmt(&mut self, stmt: &ast::IfStmt) -> CompileResult<()> {
        self.env.push_scope();
        if let Some(ref pre) = stmt.pre_stmt {
            self.compile_simple_stmt(pre)?;
        }

        self.env.pop_scope();
        unimplemented!()
    }

    fn compile_simple_stmt(&mut self, stmt: &ast::SimpleStmt) -> CompileResult<()> {
        use self::ast::SimpleStmt::*;
        match *stmt {
            Expr(ref expr) => {
                self.compile_expr(expr)?;
                self.emit(Opcode::Pop);
            }
            ShortVarDecl { ref idents, ref exprs } => {
                if idents.len() != 1 && exprs.len() != 1 {
                    unimplemented!();
                }

                let expr = &exprs[0];
                let name = idents[0];

                let ty = self.check_expr(expr)?;
                self.env.insert_decl(name, Declaration {
                    kind: DeclarationKind::Var(VarDecl { ty }),
                    span: expr.span,
                });
            }
            Assignment { ref left, ref right, op: AssignOp::None } => {

            }
            _ => unimplemented!()
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &ast::Expr) -> CompileResult<()> {
        use self::ast::ExprKind;
        match expr.kind {
            ExprKind::Literal(ref literal) => self.compile_literal(literal),
            _ => unimplemented!()
        }
    }

    fn compile_literal(&mut self, literal: &ast::Literal) -> CompileResult<()> {
        unimplemented!()
    }

    // Used for
    fn compile_mutate_expr<F>(&mut self, expr: &ast::Expr, mutate: F) -> CompileResult<()>
    where F: Fn(&mut FunctionCompiler) -> CompileResult<()>
    {
        unimplemented!()
    }

    fn compile_assignable_expr(&mut self, expr: &ast::Expr) -> CompileResult<()> {
        unimplemented!()
    }
}
