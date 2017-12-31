use ast;
use utils::result::CompileResult;
use type_check::*;

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
        for decl in source.decls {
            self.compile_top_level_decl(decl)?;
        }
    }
}

impl Compiler {

}

impl Compiler {
    fn compile_top_level_decl(&mut self, decl: &ast::TopLevelDecl) -> CompileResult<()> {
        match decl {
            TopLevelDecl::Declaration(ref decl) => {
                self.compile_decl(decl)
            }
            TopLevelDecl::Function(ref func) => {
                self.compile_func(func)
            }
            TopLevelDecl::Method(_) => unimplemented!()
        }
    }

    fn compile_decl(&mut self, decl: ast::Declaration) -> CompileResult<()> {
        unimplemented!()
    }

    fn compile_func(&mut self, func: &ast::FunctionDecl) -> CompileResult<Function> {
        assert!(func.sig.params.is_empty());
        assert!(func.sig.result == FuncResult::None);

        let body = func.body.as_ref().unwrap();
        for stmt in body {
            self.compile_stmt(stmt)?;
        }

        Ok(())
    }
}

pub struct FunctionCompiler<'env, 'fn> {
    env: &'env mut Environment,
    decl: &'fn ast::FunctionDecl,
    code: Vec<Opcode>,
    consts: Vec<Primitive>,
}

impl<'env, 'fn> FunctionCompiler<'env, 'fn> {
    fn new(env: &'env mut Environment, decl: &'fn ast::FunctionDecl) -> FunctionCompiler<'env, 'fn> {
        FunctionCompiler {
            env, decl,
            code: Vec::new(),
            consts: Vec::new(),
        }
    }

    fn compile(&mut self) -> CompileResult<Function> {
        for stmt in self.decl.body.as_ref().unwrap() {
            self.compile_stmt(stmt)?;
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct CodeOffset(u32);

impl<'env, 'fn> FunctionCompiler<'env, 'fn> {
    fn emit(&mut self, opcode: Opcode) -> CodeOffset {
        self.code.push(opcode);
        CodeOffset(self.code.len() - 1)
    }
}

impl<'env, 'fn> FunctionCompiler<'env, 'fn> {
    fn compile_stmt(&mut self, stmt: &ast::Stmt) -> CompileResult<()> {
        use self::ast::Stmt::*;
        match stmt {
            If(ref if_stmt) => {
                self.compile_if_stmt(if_stmt)?;
            }
            Simple(simple) => {
                self.compile_simple_stmt(simple)?;
            }
        }
        Ok(())
    }

    fn compile_if_stmt(&mut self, stmt: &ast::IfStmt) -> CompileResult<()> {
        if let Some(pre) = stmt.pre_stmt {
            self.compile_simple_stmt(pre)?;
        }



        Ok(())
    }
}
