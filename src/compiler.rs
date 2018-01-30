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
use utils::intern::Atom;

pub struct Compiler {
    env: Environment,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            env: Environment::new(),
        }
    }

    pub fn compile(&mut self, source: &ast::SourceFile) -> CompileResult<Function> {
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

    fn compile_top_level_functions(&mut self, source: &ast::SourceFile) -> CompileResult<Function> {
        for decl in source.decls.iter() {
            match *decl {
                ast::TopLevelDecl::Function(ref fn_decl) => {
                    if fn_decl.name.as_str() != "main" {
                        unimplemented!()
                    }
                    return self.compile_func(fn_decl)
                }
                _ => unimplemented!()
            }
        }
        err!(Span::INVALID, "main function not found")
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
    pub code: IdVec<CodeOffset, Opcode>,
    pub local_names: IdVec<LocalId, Atom>,
}

pub struct FunctionCompiler<'env, 'func> {
    env: &'env mut Environment,
    decl: &'func ast::FunctionDecl,

    code: IdVec<CodeOffset, Opcode>,
    local_names: IdVec<LocalId, Atom>,
    expr_to_type: IdVecMap<ExprId, TypeId>,
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn new(env: &'env mut Environment, decl: &'func ast::FunctionDecl) -> FunctionCompiler<'env, 'func> {
        FunctionCompiler {
            env, decl,
            code: IdVec::new(),
            local_names: IdVec::new(),
            expr_to_type: IdVecMap::new(),
        }
    }

    fn compile(mut self) -> CompileResult<Function> {
        assert!(self.decl.sig.params.is_empty());
        assert!(self.decl.sig.result.is_empty());

        let body: &[ast::Stmt] = self.decl.body.as_ref().unwrap();

        for stmt in body.iter() {
            self.compile_stmt(stmt)?;
        }

        Ok(Function {
            code: self.code,
            local_names: self.local_names,
        })
    }

    fn declare_local(&mut self, name: Atom, ty: TypeId, span: Span) -> LocalId {
        let id = self.local_names.append(name);
        self.env.insert_decl(name, Declaration {
            kind: DeclarationKind::Var(VarDecl { ty, id }),
            span,
        });
        id
    }

    fn declare_const(&mut self, name: Atom, ty: TypeId, val: Primitive, span: Span) {
        self.env.insert_decl(name, Declaration {
            kind: DeclarationKind::Const { ty, val },
            span,
        });
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct CodeOffset(u32);
impl_id!(CodeOffset);

impl CodeOffset {
    pub fn offset_to(self, dest: CodeOffset) -> JumpOffset {
        JumpOffset(dest.0.wrapping_sub(self.0))
    }

    pub fn offset_by(self, offset: JumpOffset) -> CodeOffset {
        CodeOffset(self.0.wrapping_add(offset.0))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct JumpOffset(u32);

impl JumpOffset {
    const INVALID: JumpOffset = JumpOffset(0xDEADBEEF);
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LocalId(u32);
impl_id!(LocalId);

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn emit(&mut self, opcode: Opcode) -> CodeOffset {
        self.code.append(opcode)
    }

    fn patch_jump(&mut self, origin: CodeOffset, dest: CodeOffset) {
        match self.code[origin] {
            Opcode::BranchTrue(ref mut relative) |
            Opcode::BranchFalse(ref mut relative) |
            Opcode::Jump(ref mut relative) => {
                assert_eq!(*relative, JumpOffset::INVALID,
                           "tried to patch jump that was already patched");
                *relative = origin.offset_to(dest);
            }
            opcode => panic!("Cannot patch a non-jump {:?}", opcode)
        }
    }

    fn patch_jump_to_here(&mut self, origin: CodeOffset) {
        let dest = CodeOffset(self.code.len() as u32);
        self.patch_jump(origin, dest);
    }

    fn emit_branch_false(&mut self) -> CodeOffset {
        self.emit(Opcode::BranchFalse(JumpOffset::INVALID))
    }

    fn emit_branch_true(&mut self) -> CodeOffset {
        self.emit(Opcode::BranchTrue(JumpOffset::INVALID))
    }

    fn emit_jump(&mut self) -> CodeOffset {
        self.emit(Opcode::Jump(JumpOffset::INVALID))
    }
}

///
/// Type checking
///
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

            Equal | NotEqual | Less | LessOrEqual | Greater | GreaterOrEqual => {
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
                self.compile_simple_stmt(simple, Span::INVALID)
            }
            _ => unimplemented!()
        }
    }

    fn compile_if_stmt(&mut self, stmt: &ast::IfStmt) -> CompileResult<()> {
        self.env.push_scope();
        if let Some(ref pre) = stmt.pre_stmt {
            self.compile_simple_stmt(pre, Span::INVALID)?;
        }

        self.env.pop_scope();
        unimplemented!()
    }

    fn compile_simple_stmt(&mut self, stmt: &ast::SimpleStmt, span: Span) -> CompileResult<()> {
        use self::ast::SimpleStmt::*;
        match *stmt {
            Expr(ref expr) => {
                self.compile_expr(expr)?;
                self.emit(Opcode::Pop);
            }
            ShortVarDecl { ref idents, ref exprs } => {
                if idents.len() != 1 || exprs.len() != 1 {
                    unimplemented!();
                }

                let expr = &exprs[0];
                let name = idents[0];

                if self.env.get_local_var_decl(name).is_some() {
                    return err!(span, "no new variable on the left side of :=");
                }

                let ty = self.check_expr(expr)?;
                let id = self.declare_local(name, ty, span);

                self.compile_expr(expr)?;
                self.emit(Opcode::StoreLocal(id));
            }
            Assignment { ref left, ref right, op: AssignOp::None } => {
                if left.len() != 1 { unimplemented!() }
                if right.len() != 1 { unimplemented!() }
                let expr = &right[0];

                if let ast::ExprKind::Literal(ast::Literal::Ident(name)) = left[0].kind {
                    let expr_ty = self.check_expr(expr)?;
                    self.compile_expr(expr)?;

                    let opcode = match self.env.get_decl(name).map(|d| &d.kind) {
                        Some(&DeclarationKind::Var(VarDecl { ty, id })) => {
                            if ty.resolve_aliases(&self.env) != expr_ty {
                                // TODO: Pretty print expression
                                return err!(span,
                                    "cannot use {:?} (of type {}) as type {} in assignment",
                                    expr.kind, expr_ty.name(&self.env), ty.name(&self.env));
                            }
                            Opcode::StoreLocal(id)
                        }
                        Some(&DeclarationKind::Const { .. }) => {
                            return err!(expr.span, "cannot assign to {}", name);
                        }
                        Some(&DeclarationKind::Type(_)) => {
                            return err!(expr.span, "type {} is not an expression", name);
                        }
                        None => {
                            return err!(expr.span, "use of undeclared identifier {}", name);
                        }
                    };
                    self.emit(opcode);
                } else {
                    unimplemented!("can only assign to identifier")
                }
            }
            _ => unimplemented!()
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &ast::Expr) -> CompileResult<()> {
        use ast::ExprKind::*;
        let ty = self.check_expr(expr)?;
        match expr.kind {
            Literal(ref literal) => {
                self.compile_literal(literal, expr.span)?;
            }
            Binary { ref left, ref right, op: ast::BinaryOp::LogAnd } => {
                // Evaluate left expr, leaving a bool on the stack
                self.compile_expr(left)?;
                // Duplicate the bool; the branch consumes one, then if it branches we have the
                // other on the stack being the result of this expression
                self.emit(Opcode::Dup);
                // If the bool on the stack is false, branch to skip evaluation of the right expr
                let origin = self.emit_branch_false();
                // If we didn't branch, we need to remove the useless `true` off the stack
                self.emit(Opcode::Pop);
                // Evaluate right expr, leaving a bool on the stack
                self.compile_expr(right)?;
                // Make the above branch point to after the evaluation of the right expr
                self.patch_jump_to_here(origin);
            }
            Binary { ref left, ref right, op: ast::BinaryOp::LogOr } => {
                // Evaluate left expr, leaving a bool on the stack
                self.compile_expr(left)?;
                // Duplicate the bool; the branch consumes one, then if it branches we have the
                // other on the stack being the result of this expression
                self.emit(Opcode::Dup);
                // If the bool on the stack is true, branch to skip evaluation of the right expr
                let origin = self.emit_branch_true();
                // If we didn't branch, we need to remove the useless `false` off the stack
                self.emit(Opcode::Pop);
                // Evaluate right expr, leaving a bool on the stack
                self.compile_expr(right)?;
                // Make the above branch point to after the evaluation of the right expr
                self.patch_jump_to_here(origin);
            }
            Binary { ref left, ref right, op } => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;

                use ast::BinaryOp::*;
                use vm::IntegerBinaryOp;
                let op = match op {
                    Equal => IntegerBinaryOp::Equal,
                    NotEqual => IntegerBinaryOp::NotEqual,
                    Less => IntegerBinaryOp::Less,
                    LessOrEqual => IntegerBinaryOp::LessOrEqual,
                    Greater => IntegerBinaryOp::Greater,
                    GreaterOrEqual => IntegerBinaryOp::GreaterOrEqual,
                    Add => IntegerBinaryOp::Add,
                    Sub => IntegerBinaryOp::Sub,
                    Or => IntegerBinaryOp::Or,
                    Xor => IntegerBinaryOp::Xor,
                    Mul => IntegerBinaryOp::Mul,
                    Div => IntegerBinaryOp::Div,
                    Remainder => IntegerBinaryOp::Remainder,
                    LShift => IntegerBinaryOp::LShift,
                    RShift => IntegerBinaryOp::RShift,
                    And => IntegerBinaryOp::And,
                    AndNot => IntegerBinaryOp::AndNot,

                    // Covered by other `Binary` cases in the outer match block
                    LogAnd | LogOr => unreachable!()
                };

                let ty = ty.as_primitive_type(&self.env);
                self.emit(Opcode::IntegerBinary { op, ty });
            }
            _ => unimplemented!()
        }
        Ok(())
    }

    fn compile_literal(&mut self, literal: &ast::Literal, span: Span) -> CompileResult<()> {
        use ast::Literal::*;
        match *literal {
            Ident(name) => {
                let opcode = match self.env.get_decl(name).map(|d| &d.kind) {
                    None => {
                        return err!(span, "use of undeclared identifier {}", name);
                    }
                    Some(&DeclarationKind::Var(VarDecl { id, .. })) => {
                        Opcode::LoadLocal(id)
                    }
                    Some(&DeclarationKind::Const { val, .. }) => {
                        Opcode::PushConst(val)
                    }
                    Some(&DeclarationKind::Type(_)) => {
                        return err!(span, "type {} is not an expression", name);
                    }
                };
                self.emit(opcode);
            }
            Int(n) => {
                self.emit(Opcode::PushConst(Primitive { u64: n }));
            }
            _ => unimplemented!()
        }
        Ok(())
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
