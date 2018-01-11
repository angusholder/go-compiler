use ast;
use utils::result::CompileResult;
use type_check::*;
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
        unimplemented!()
    }

    fn compile_top_level_initializers(&mut self, source: &ast::SourceFile) -> CompileResult<()> {
        unimplemented!()
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
    code: Vec<Opcode>,
    consts: Vec<Primitive>,
}

pub struct FunctionCompiler<'env, 'func> {
    env: &'env mut Environment,
    decl: &'func ast::FunctionDecl,
    code: Vec<Opcode>,
    consts: Vec<Primitive>,
}

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn new(env: &'env mut Environment, decl: &'func ast::FunctionDecl) -> FunctionCompiler<'env, 'func> {
        FunctionCompiler {
            env, decl,
            code: Vec::new(),
            consts: Vec::new(),
        }
    }

    fn compile(mut self) -> CompileResult<Function> {
        assert!(self.decl.sig.params.is_empty());
//        assert!(self.decl.sig.result == ast::FuncResult::None);

        let body = self.decl.body.as_ref().unwrap();

        for stmt in body.iter() {
            self.compile_stmt(stmt)?;
        }

        Ok(Function {
            code: self.code,
            consts: self.consts,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct CodeOffset(u32);

impl<'env, 'func> FunctionCompiler<'env, 'func> {
    fn emit(&mut self, opcode: Opcode) -> CodeOffset {
        self.code.push(opcode);
        CodeOffset(self.code.len() as u32 - 1)
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
        if let Some(ref pre) = stmt.pre_stmt {
            self.compile_simple_stmt(pre)?;
        }

        unimplemented!()
    }

    fn compile_simple_stmt(&mut self, stmt: &ast::SimpleStmt) -> CompileResult<()> {
        use self::ast::SimpleStmt::*;
        match *stmt {
            Expr(ref expr) => self.compile_expr(expr),
            _ => unimplemented!()
        }
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
