use ast;
use scope::*;
use token::AssignOp;
use types::TypeId;
use types;
use utils::id::{ IdVec, IdVecMap };
use utils::intern::Atom;
use utils::result::{ CompileResult, Span };
use vm::{ Opcode, Primitive };

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
        use ast::Declaration::*;
        use ast::TopLevelDecl::*;
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
        use ast::TopLevelDecl::*;
        match *decl {
            Declaration(ref decl) => {
                unimplemented!()
            }
            Function(ref func) => {
                self.compile_func(func)?;
                unimplemented!()
            }
            Method(_) => unimplemented!()
        }
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
    expr_to_type: IdVecMap<ast::ExprId, TypeId>,
    jump_target_locs: IdVecMap<ast::JumpTargetId, CodeOffset>,
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn new(env: &'env mut Environment, decl: &'func ast::FunctionDecl) -> FunctionCompiler<'env, 'func> {
        FunctionCompiler {
            env,
            decl,
            code: IdVec::new(),
            local_names: IdVec::new(),
            expr_to_type: IdVecMap::new(),
            jump_target_locs: IdVecMap::new(),
        }
    }

    fn compile(mut self) -> CompileResult<Function> {
        assert!(self.decl.sig.params.is_empty());
        assert!(self.decl.sig.result.is_empty());

        let block = self.decl.body.as_ref().unwrap();

        self.compile_block(block)?;

        self.code.append(Opcode::Return);

        Ok(Function {
            code: self.code,
            local_names: self.local_names,
        })
    }

    fn declare_local(&mut self, name: Atom, ty: TypeId, span: Span) -> LocalId {
        let id = self.local_names.append(name);
        self.env.insert_decl(name, Declaration::Var { ty, id, span });
        id
    }

    fn declare_const(&mut self, name: Atom, ty: TypeId, val: Primitive, span: Span) {
        self.env.insert_decl(name, Declaration::Const { ty, val, span });
    }
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn compile_decl(&mut self, decl: &ast::Declaration) -> CompileResult<()> {
        match *decl {
            ast::Declaration::Var(ref var_specs) => {
                for spec in var_specs.iter() {
                    self.compile_var_spec(spec)?;
                }
            }
            ast::Declaration::Type(ref type_specs) => {
                unimplemented!()
            }
            ast::Declaration::Const(ref const_specs) => {
                unimplemented!()
            }
        }
        Ok(())
    }

    fn compile_var_spec(&mut self, spec: &ast::VarSpec) -> CompileResult<()> {
        let idents = &spec.idents[..];
        match (spec.ty.as_ref(), spec.exprs.as_ref()) {
            (Some(ty), Some(exprs)) => {
                self.compile_var_decl_with_ty_and_exprs(idents, ty, exprs, spec.span)
            }
            (Some(ty), None) => {
                self.compile_var_decl_with_ty(idents, ty, spec.span)
            }
            (None, Some(exprs)) => {
                self.compile_var_decl_with_exprs(idents, exprs, spec.span)
            }
            // The parser shouldn't allow this
            (None, None) => unreachable!()
        }
    }

    fn compile_var_decl_with_ty(&mut self,
                                idents: &[Atom],
                                ty: &ast::Type,
                                span: Span) -> CompileResult<()> {
        let ty = self.check_type(ty)?;
        for &name in idents {
            if let Some(decl) = self.env.local_decl(name) {
                return err!(span, "{} redeclared in this block, previous declaration at {:?}",
                            name, decl.span());
            }
            self.declare_local(name, ty, span);
        }
        Ok(())
    }

    fn compile_var_decl_with_ty_and_exprs(&mut self,
                                          idents: &[Atom],
                                          ty: &ast::Type,
                                          exprs: &[ast::Expr],
                                          span: Span) -> CompileResult<()> {
        self.compile_var_decl_with_ty(idents, ty, span)?;
        unimplemented!()
    }

    fn compile_var_decl_with_exprs(&mut self,
                                   idents: &[Atom],
                                   exprs: &[ast::Expr],
                                   span: Span) -> CompileResult<()> {
        unimplemented!()
    }

    fn compile_short_var_decl(&mut self,
                              idents: &[Atom],
                              exprs: &[ast::Expr],
                              span: Span) -> CompileResult<()> {
        if !idents.iter().any(|&name| self.env.local_decl(name).is_none()) {
            return err!(span, "no new variables on left side of :=");
        }

        // TODO: use `self.compile_var_decl_with_exprs(idents, exprs, span)`

        if idents.len() != exprs.len() {
            unimplemented!();
        }

        for (&name, expr) in idents.iter().zip(exprs) {
            if self.env.local_decl(name).is_some() {
                return err!(span, "no new variable on the left side of :=");
            }

            let ty = self.check_expr(expr)?;
            let id = self.declare_local(name, ty, span);

            self.compile_expr(expr)?;
            self.emit(Opcode::StoreLocal(id));
        }

        Ok(())
    }

    fn compile_assignment(&mut self, left: &[ast::Expr], right: &[ast::Expr]) -> CompileResult<()> {
        if left.len() != right.len() {
            unimplemented!();
        }

        for (left_expr, right_expr) in left.iter().zip(right) {
            let right_ty = self.check_expr(right_expr)?;
            self.compile_expr(right_expr)?;

            if let ast::ExprKind::Literal(ast::Literal::Ident(name)) = left_expr.kind {
                let left_ty = self.check_expr(left_expr)?;
                self.compile_expr(left_expr)?;

                let opcode = match self.env.decl(name) {
                    Some(&Declaration::Var { id, .. }) => {
                        if !right_ty.assignable_to(left_ty, &self.env) {
                            // TODO: Pretty print expression
                            return err!(right_expr.span,
                                "cannot use {:?} (of type {}) as type {} in assignment",
                                right_expr.kind, right_ty.name(&self.env), left_ty.name(&self.env));
                        }
                        Opcode::StoreLocal(id)
                    }
                    Some(&Declaration::Const { .. }) => {
                        return err!(left_expr.span, "cannot assign to {}", name);
                    }
                    Some(&Declaration::Type { .. }) => {
                        return err!(left_expr.span, "type {} is not an expression", name);
                    }
                    None => {
                        return err!(left_expr.span, "use of undeclared identifier {}", name);
                    }
                };
                self.emit(opcode);
            } else {
                unimplemented!("can only assign to identifier")
            }
        }
        Ok(())
    }
}

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

    fn emit_jump_to(&mut self, dest: CodeOffset) {
        let here = self.next_code_offset();
        let jump_offset = here.offset_to(dest);
        self.emit(Opcode::Jump(jump_offset));
    }

    /// The `CodeOffset` that will point to location where the next opcode will be emitted
    fn next_code_offset(&self) -> CodeOffset {
        CodeOffset(self.code.len() as u32)
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
        use ast::BinaryOp::*;
        match op {
            LogOr | LogAnd => {
                if !left_ty.is_boolean(&self.env) || !right_ty.is_boolean(&self.env) {
                    return err!(span, "{} operator expects boolean operands, got {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                if left_ty != right_ty {
                    return err!(span, "invalid operation {}, mismatched types {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                Ok(left_ty)
            }

            Equal | NotEqual | Less | LessOrEqual | Greater | GreaterOrEqual => {
                if left_ty != right_ty {
                    return err!(span, "invalid operation {}, mismatched types {} and {}",
                                    op, left_ty.name(&self.env), right_ty.name(&self.env));
                }

                Ok(types::UNTYPED_BOOL)
            }

            Add | Sub | Or | Xor | Mul | Div | Remainder | And | AndNot => {
                if left_ty != right_ty {
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

    fn check_unary_op_expr(&mut self,
                           op: ast::UnaryOp,
                           expr_ty: TypeId,
                           span: Span) -> CompileResult<TypeId> {
        use ast::UnaryOp;
        match op {
            UnaryOp::LogNot => {
                if !expr_ty.is_boolean(&self.env) {
                    return err!(span, "{} operator expects boolean operand, got {}",
                                    op, expr_ty.name(&self.env));
                }
                Ok(expr_ty)
            }
            UnaryOp::Not => {
                if !expr_ty.is_integer(&self.env) {
                    return err!(span, "{} operator expects integer operand, got {}",
                                    op, expr_ty.name(&self.env));
                }
                Ok(expr_ty)
            }
            UnaryOp::Plus | UnaryOp::Minus => {
                if !expr_ty.is_number(&self.env) {
                    return err!(span, "{} operator expects number operand, got {}",
                                    op, expr_ty.name(&self.env));
                }
                Ok(expr_ty)
            }
            _ => unimplemented!()
        }
    }

    fn check_expr(&mut self, expr: &ast::Expr) -> CompileResult<TypeId> {
        use ast::ExprKind::*;

        if let Some(&ty) = self.expr_to_type.get(expr.id) {
            return Ok(ty);
        }

        let result_type = match expr.kind {
            Binary { op, ref left, ref right } => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;

                self.check_binary_op_expr(op, left_ty, right_ty, expr.span)?
            }
            Unary { op, ref expr } => {
                let expr_ty = self.check_expr(expr)?;
                self.check_unary_op_expr(op, expr_ty, expr.span)?
            }
            Literal(ast::Literal::Int(_)) => {
                types::UNTYPED_INT
            }
            Literal(ast::Literal::Ident(ident)) => {
                match self.env.decl(ident) {
                    Some(&Declaration::Var { ty, .. }) => ty,
                    Some(&Declaration::Const { ty, .. }) => ty,
                    Some(&Declaration::Type { ty, .. }) => {
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

    /// Type check an expression and assert that it yields exactly one value
    /// May be used for example on the condition of an if statement
    fn check_single_expr(&mut self, expr: &ast::Expr) -> CompileResult<TypeId> {
        unimplemented!()
    }

    /// Type check an expression that can yield any number of values
    /// For example: `a, b = f()` or `i, ok = someInterface.(int)`
    fn check_multi_expr(&mut self, expr: &ast::Expr) -> CompileResult<Vec<TypeId>> {
        unimplemented!()
    }

    fn check_type(&mut self, ty: &ast::Type) -> CompileResult<TypeId> {
        if let ast::Type::TypeName { ident, package: None } = *ty {
            let decl = self.env.decl(ident);
            match decl {
                None => {
                    unimplemented!()
                }
                Some(&Declaration::Type { ty, .. }) => Ok(ty),
                Some(&Declaration::Const { .. }) => {
                    unimplemented!()
                }
                Some(&Declaration::Var { .. }) => {
                    unimplemented!()
                }
            }
        } else {
            unimplemented!()
        }
    }
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn compile_block(&mut self, block: &ast::Block) -> CompileResult<()> {
        for stmt in block.iter() {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &ast::Stmt) -> CompileResult<()> {
        use ast::Stmt::*;
        match *stmt {
            If(ref if_stmt) => {
                self.compile_if_stmt(if_stmt)
            }
            Simple(ref simple) => {
                self.compile_simple_stmt(simple, Span::INVALID)
            }
            For(ref for_stmt) => {
                self.compile_for_stmt(for_stmt)
            }
            Declaration(ref decl) => {
                self.compile_decl(decl)
            }
            Labeled { name, id, ref stmt } => {
                let offset = self.next_code_offset();
                self.jump_target_locs[id] = offset;
                if let Some(stmt) = stmt.as_ref() {
                    self.compile_stmt(stmt)
                } else {
                    Ok(())
                }
            }
            _ => unimplemented!()
        }
    }

    fn compile_for_stmt(&mut self, stmt: &ast::ForStmt) -> CompileResult<()> {
        use ast::ForStmtHeader::*;
        match stmt.header {
            Always => {
                let loop_top = self.next_code_offset();
                self.jump_target_locs[stmt.continue_target] = loop_top;

                self.env.push_scope();
                self.compile_block(&stmt.body)?;
                self.env.pop_scope();
                self.emit_jump_to(loop_top);

                let loop_end = self.next_code_offset();
                self.jump_target_locs[stmt.break_target] = loop_end;
            }
            ForClause { ref init_stmt, ref cond, ref post_stmt } => {
                self.compile_for_clause(stmt,
                        init_stmt.as_ref().map(|s| &**s),
                        cond.as_ref().map(|s| &**s),
                        post_stmt.as_ref().map(|s| &**s))?;
            }
            RangeClauseAssign { .. } => {
                unimplemented!()
            }
            RangeClauseDeclAssign { .. } => {
                unimplemented!()
            }
        }
        Ok(())
    }

    fn compile_for_clause(&mut self,
                          stmt: &ast::ForStmt,
                          init_stmt: Option<&ast::SimpleStmt>,
                          cond: Option<&ast::Expr>,
                          post_stmt: Option<&ast::SimpleStmt>) -> CompileResult<()> {
        self.env.push_scope();

        if let Some(init_stmt) = init_stmt {
            self.compile_simple_stmt(init_stmt, Span::INVALID)?;
        }

        let loop_start = self.next_code_offset();
        self.jump_target_locs[stmt.continue_target] = loop_start;

        if let Some(cond) = cond {
            self.compile_expr(cond)?;
            let branch_over_body = self.emit_branch_false();
            self.compile_block(&stmt.body)?;

            if let Some(post_stmt) = post_stmt {
                self.compile_simple_stmt(post_stmt, Span::INVALID)?;
            }

            self.emit_jump_to(loop_start);
            self.patch_jump_to_here(branch_over_body);
        } else {
            self.compile_block(&stmt.body)?;

            if let Some(post_stmt) = post_stmt {
                self.compile_simple_stmt(post_stmt, Span::INVALID)?;
            }

            self.emit_jump_to(loop_start);
        }

        let loop_end = self.next_code_offset();
        self.jump_target_locs[stmt.break_target] = loop_end;

        self.env.pop_scope();

        Ok(())
    }

    fn compile_if_stmt(&mut self, stmt: &ast::IfStmt) -> CompileResult<()> {
        self.env.push_scope();
        if let Some(ref pre) = stmt.pre_stmt {
            self.compile_simple_stmt(pre, Span::INVALID)?;
        }

        let cond_ty = self.check_expr(&stmt.cond)?;
        if cond_ty.underlying_type(&self.env) != types::BOOL {
            return err!(Span::INVALID, "non-bool {:?} (type {}) used as if condition",
                        stmt.cond, cond_ty.name(&self.env));
        }

        self.compile_expr(&stmt.cond)?;
        let branch_over_then = self.emit_branch_false();

        self.compile_block(&stmt.then)?;

        match stmt.els {
            ast::IfStmtTail::Else(ref block) => {
                let jump_over_else = self.emit_jump();
                self.patch_jump_to_here(branch_over_then);
                self.compile_block(block)?;
                self.patch_jump_to_here(jump_over_else);
            }
            ast::IfStmtTail::None => {
                self.patch_jump_to_here(branch_over_then);
            }
            ast::IfStmtTail::ElseIf(ref else_if) => {
                let jump_over_elseif = self.emit_jump();
                self.patch_jump_to_here(branch_over_then);
                self.compile_if_stmt(else_if)?;
                self.patch_jump_to_here(jump_over_elseif);
            }
        }

        self.env.pop_scope();

        Ok(())
    }

    fn compile_simple_stmt(&mut self, stmt: &ast::SimpleStmt, span: Span) -> CompileResult<()> {
        use ast::SimpleStmt::*;
        match *stmt {
            Expr(ref expr) => {
                self.compile_expr(expr)?;
                self.emit(Opcode::Pop);
            }
            ShortVarDecl { ref idents, ref exprs } => {
                self.compile_short_var_decl(idents, exprs, span)?;
            }
            Assignment { ref left, ref right } => {
                self.compile_assignment(left, right)?;
            }
            _ => unimplemented!()
        }

        Ok(())
    }
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
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
            Unary { ref expr, op } => {
                self.compile_expr(expr)?;

                use ast::UnaryOp;
                use vm::IntegerUnaryOp;
                let op = match op {
                    UnaryOp::Minus => IntegerUnaryOp::Minus,
                    UnaryOp::LogNot => IntegerUnaryOp::LogNot,
                    UnaryOp::Not => IntegerUnaryOp::Not,
                    _ => unimplemented!(),
                };

                let ty = ty.as_primitive_type(&self.env);
                self.emit(Opcode::IntegerUnary { op, ty });
            }
            _ => unimplemented!()
        }
        Ok(())
    }

    /// Compile an expression yielding one or more values, discard all but one
    fn compile_single_expr_truncate(&mut self, expr: &ast::Expr) -> CompileResult<()> {
        unimplemented!()
    }

    fn compile_literal(&mut self, literal: &ast::Literal, span: Span) -> CompileResult<()> {
        use ast::Literal::*;
        match *literal {
            Ident(name) => {
                let opcode = match self.env.decl(name) {
                    None => {
                        return err!(span, "use of undeclared identifier {}", name);
                    }
                    Some(&Declaration::Var { id, .. }) => {
                        Opcode::LoadLocal(id)
                    }
                    Some(&Declaration::Const { val, .. }) => {
                        Opcode::PushConst(val)
                    }
                    Some(&Declaration::Type { .. }) => {
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
pub struct JumpOffset(pub u32);

impl JumpOffset {
    const INVALID: JumpOffset = JumpOffset(0xDEADBEEF);
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LocalId(u32);
impl_id!(LocalId);