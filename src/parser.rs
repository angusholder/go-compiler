use ast::*;
use lexer::{ Lexer, Token, TokenKind, Keyword };
use utils::ptr::{ List, P };
use utils::result::{ CompileResult, Span };

struct Parser<'src> {
    lexer: Lexer<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Parser<'src> {
        Parser {
            lexer: Lexer::new(src)
        }
    }

    pub fn parse(&mut self) -> CompileResult<SourceFile> {
        self.lexer.expect_keyword(Keyword::Package)?;
        let package_name = self.lexer.expect_ident()?;

        let mut imports: Vec<ImportSpec> = Vec::new();
        while self.lexer.match_keyword(Keyword::Import)? {
            imports.extend(self.parse_import_decl()?.decls);
        }

        let mut decls: Vec<TopLevelDecl> = Vec::new();
        while let Some(decl) = self.parse_top_level_decl()? {
            decls.push(decl);
        }

        let next_token = self.lexer.next()?;
        if next_token != None {
            return err!(next_token, "expected top level declaration or end of file, got {:#?}",
                next_token);
        }

        Ok(SourceFile {
            package_name,
            imports: imports.into(),
            decls: decls.into(),
        })
    }
}

impl<'src> Parser<'src> {
    fn parse_import_decl(&mut self) -> CompileResult<ImportDecl> {
        let mut specs = Vec::new();
        if self.lexer.match_token(TokenKind::LParen)? {
            while !self.lexer.match_token(TokenKind::RParen)? {
                specs.push(self.parse_import_spec()?);
                self.expect_terminal()?;
            }
        } else {
            specs.push(self.parse_import_spec()?);
            self.expect_terminal()?;
        }
        Ok(ImportDecl {
            decls: specs.into()
        })
    }

    fn parse_import_spec(&mut self) -> CompileResult<ImportSpec> {
        let alias: ImportAlias;
        if self.lexer.match_token(TokenKind::Dot)? {
            alias = ImportAlias::Splat;
        } else if let Some(ident) = self.lexer.match_ident()? {
            alias = ImportAlias::Name(ident);
        } else {
            alias = ImportAlias::None;
        }

        let path = self.lexer.expect_string_lit()?;

        Ok(ImportSpec { alias, path })
    }

    fn parse_top_level_decl(&mut self) -> CompileResult<Option<TopLevelDecl>> {
        use self::Keyword::*;

        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            return Ok(None);
        };

        match token.kind {
            TokenKind::Keyword(Func) => {
                Ok(Some(TopLevelDecl::Function(self.parse_func()?)))
            }
            TokenKind::Keyword(Const) | TokenKind::Keyword(Type) | TokenKind::Keyword(Var) => {
                unimplemented!()
            }
            _ => {
                err!(token, "expected top level declaration (const/type/var/func), found {:#?}",
                    token.kind)
            }
        }
    }

    fn parse_func(&mut self) -> CompileResult<FunctionDecl> {
        let name = self.lexer.expect_ident()?;
        let sig = self.parse_signature()?;
        let body = if self.lexer.match_token(TokenKind::LBrace)? {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(FunctionDecl { name, sig, body })
    }

    fn parse_signature(&mut self) -> CompileResult<Signature> {
        let mut params = Vec::new();
        while !self.lexer.match_token(TokenKind::RParen)? {
            params.push(self.parse_parameter_decl()?);
            if !self.lexer.match_token(TokenKind::Comma)? {
                self.lexer.expect_token(TokenKind::RParen)?;
                break;
            }
        }

        let result: FuncResult;
        if self.lexer.match_token(TokenKind::LParen)? {
            result = FuncResult::Many(self.parse_parameter_decl()?);
        }
        // These tokens signify the end of the signature and the start of the function body
        // respectively, signalling that there is no return type specified
        else if self.lexer.peek_match_token(TokenKind::Semicolon)?
            || self.lexer.peek_match_token(TokenKind::LBrace)? {
            result = FuncResult::None;
        } else {
            result = FuncResult::One(self.parse_type()?);
        }

        Ok(Signature { params: params.into(), result })
    }

    fn parse_parameter_decl(&mut self) -> CompileResult<ParameterDecl> {
        let idents: Option<List<Ident>>;

        if let Some(ident) = self.lexer.match_ident()? {
            let mut list = vec![ident];
            while self.lexer.match_token(TokenKind::Comma)? {
                list.push(self.lexer.expect_ident()?);
            }
            idents = Some(list.into());
        } else {
            idents = None;
        }

        let ellipsis = self.lexer.match_token(TokenKind::Ellipsis)?;
        let ty = self.parse_type()?;

        Ok(ParameterDecl {
            idents: idents.into(),
            ellipsis,
            ty,
        })
    }

    fn parse_opt_type_with_token(&mut self, token: Token) -> CompileResult<Option<P<Type>>> {
        Ok(Some(match token.kind {
            TokenKind::Ident(package_or_ident) => {
                if self.lexer.match_token(TokenKind::Dot)? {
                    let ident = self.lexer.expect_ident()?;
                    P(Type::TypeName { package: Some(package_or_ident), ident, })
                } else {
                    P(Type::TypeName { package: None, ident: package_or_ident })
                }
            }
            TokenKind::LParen => {
                let ty = self.parse_type()?;
                self.lexer.expect_token(TokenKind::RParen)?;
                ty
            }
            TokenKind::LBracket => {
                if self.lexer.match_token(TokenKind::RBracket)? {
                    P(Type::TypeLit(TypeLit::SliceType(self.parse_type()?)))
                } else {
                    let length = P(self.parse_expr()?);
                    self.lexer.expect_token(TokenKind::RBracket)?;
                    let elem_ty = self.parse_type()?;
                    P(Type::TypeLit(TypeLit::ArrayType { length, elem_ty }))
                }
            }
            TokenKind::Star => {
                P(Type::TypeLit(TypeLit::PointerType(self.parse_type()?)))
            }
            TokenKind::Keyword(Keyword::Func) => {
                P(Type::TypeLit(TypeLit::FunctionType(self.parse_signature()?)))
            }
            TokenKind::Keyword(Keyword::Map) => {
                self.lexer.expect_token(TokenKind::LBracket)?;
                let key_ty = self.parse_type()?;
                self.lexer.expect_token(TokenKind::RBracket)?;
                let elem_ty = self.parse_type()?;
                P(Type::TypeLit(TypeLit::MapType { key_ty, elem_ty }))
            }
            token @ TokenKind::Keyword(Keyword::Chan) | token @ TokenKind::SendReceive => {
                let direction: ChannelDirection;
                if token == TokenKind::SendReceive {
                    self.lexer.expect_keyword(Keyword::Chan)?;
                    direction = ChannelDirection::Receive;
                } else if self.lexer.match_token(TokenKind::SendReceive)? {
                    direction = ChannelDirection::Send;
                } else {
                    direction = ChannelDirection::BiDirectional;
                }

                let elem_ty = self.parse_type()?;

                P(Type::TypeLit(TypeLit::ChannelType { direction, elem_ty }))
            }
            TokenKind::Keyword(Keyword::Struct) | TokenKind::Keyword(Keyword::Interface) => {
                unimplemented!()
            }
            other => {
                self.lexer.unget(Some(Token { kind: other, span: token.span }));
                return Ok(None);
            }
        }))
    }

    fn parse_opt_type(&mut self) -> CompileResult<Option<P<Type>>> {
        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            return Ok(None);
        };
        self.parse_opt_type_with_token(token)
    }

    fn parse_type(&mut self) -> CompileResult<P<Type>> {
        if let Some(ty) = self.parse_opt_type()? {
            Ok(ty)
        } else {
            let next = self.lexer.next()?;
            err!(next, "expected type, got {:#?}", next)
        }
    }
}



//
// Expression parsing
//
const BP_PARENS: i32 = 0;      // ( <expr> )
const BP_COMMA: i32 = 1;       // ,
const BP_LOGOR: i32 = 10;      // ||
const BP_LOGAND: i32 = 20;     // &&
const BP_COMPARISON: i32 = 30; // ==  !=  <  <=  >  >=
const BP_ADD: i32 = 40;        // +  -  |  ^
const BP_MUL: i32 = 50;        // *  /  %  <<  >>  &  &^
const BP_UNARY: i32 = 60;      // + - ! ^ * & <-
const BP_ACCESSOR: i32 = 70;   // a.b a[b] a[b:c] a.(type) f(a, b) []type(a)

type NullDenotation = fn(p: &mut Parser, token: Token, bp: i32) -> CompileResult<Expr>;
type LeftDenotation = fn(p: &mut Parser, token: Token, left: Expr, rbp: i32) -> CompileResult<Expr>;

fn null_constant(_p: &mut Parser, token: Token, _bp: i32) -> CompileResult<Expr> {
    Ok(Expr::Literal(match token.kind {
        TokenKind::Ident(i) => Literal::Ident(i),
        TokenKind::Integer(n) => Literal::Int(n),
        TokenKind::StrLit(s) => Literal::String(s),
        _ => unreachable!(),
    }))
}

fn null_paren(p: &mut Parser, _token: Token, bp: i32) -> CompileResult<Expr> {
    let result = p.parse_expr_until(bp)?;
    p.lexer.expect_token(TokenKind::RParen)?;
    Ok(result)
}

fn null_prefix_op(p: &mut Parser, token: Token, bp: i32) -> CompileResult<Expr> {
    let child = p.parse_expr_until(bp)?;
    let op = match token.kind {
        TokenKind::Plus => UnaryOp::Plus,
        TokenKind::Minus => UnaryOp::Minus,
        TokenKind::LogNot => UnaryOp::LogNot,
        TokenKind::Caret => UnaryOp::Not,
        TokenKind::Star => UnaryOp::Deref,
        TokenKind::And => UnaryOp::AddrOf,
        TokenKind::SendReceive => UnaryOp::Receive,
        _ => unreachable!(),
    };

    Ok(Expr::Unary { op, child: P(child) })
}

fn left_index(p: &mut Parser, token: Token, left: Expr, _rbp: i32) -> CompileResult<Expr> {
    let right = p.parse_opt_expr()?;
    if p.lexer.match_token(TokenKind::Colon)? {
        let start = right;
        let end = p.parse_opt_expr()?;
        let mut max: Option<Expr> = None;
        if p.lexer.match_token(TokenKind::Colon)? {
            max = p.parse_opt_expr()?;
        }
        p.lexer.expect_token(TokenKind::RBracket)?;
        Ok(Expr::Slice {
            left: P(left),
            start: start.map(P),
            end: end.map(P),
            max: max.map(P)
        })
    } else {
        if let Some(right) = right {
            p.lexer.expect_token(TokenKind::RBracket)?;
            Ok(Expr::Index { left: P(left), right: P(right) })
        } else {
            err!(token, "expected expression")
        }
    }
}

fn left_selector(p: &mut Parser, _token: Token, left: Expr, _rbp: i32) -> CompileResult<Expr> {
    if p.lexer.match_token(TokenKind::LParen)? {
        // Type Assertion
        let ty = p.parse_type()?;
        p.lexer.expect_token(TokenKind::RParen)?;
        Ok(Expr::TypeAssertion { left: P(left), ty })
    } else {
        // Field Access
        let field_name = p.lexer.expect_ident()?;
        Ok(Expr::Selector { left: P(left), field_name })
    }
}

fn left_binary_op(p: &mut Parser, token: Token, left: Expr, rbp: i32) -> CompileResult<Expr> {
    let op = match token.kind {
        TokenKind::Plus => BinaryOp::Add,
        TokenKind::Minus => BinaryOp::Sub,
        TokenKind::Star => BinaryOp::Mul,
        TokenKind::Slash => BinaryOp::Div,
        TokenKind::Percent => BinaryOp::Remainder,

        TokenKind::And => BinaryOp::And,
        TokenKind::Or => BinaryOp::Or,
        TokenKind::Caret => BinaryOp::Xor,
        TokenKind::AndNot => BinaryOp::AndNot,

        TokenKind::LShift => BinaryOp::LShift,
        TokenKind::RShift => BinaryOp::RShift,

        TokenKind::Less => BinaryOp::Less,
        TokenKind::LessOrEqual => BinaryOp::LessOrEqual,
        TokenKind::Greater => BinaryOp::Greater,
        TokenKind::GreaterOrEqual => BinaryOp::GreaterOrEqual,
        TokenKind::Equals => BinaryOp::Equals,
        TokenKind::NotEqual => BinaryOp::NotEqual,

        TokenKind::LogAnd => BinaryOp::LogAnd,
        TokenKind::LogOr => BinaryOp::LogOr,
        _ => panic!("Unexpected token {:?}", token),
    };

    let right = p.parse_expr_until(rbp)?;

    Ok(Expr::Binary { op, left: P(left), right: P(right) })
}

fn left_func_call(p: &mut Parser, _token: Token, left: Expr, _rbp: i32) -> CompileResult<Expr> {
    let mut args = Vec::new();
    let mut ty: Option<P<Type>> = None;
    let mut ellipsis = false;

    if !p.lexer.match_token(TokenKind::RParen)? {
        let arg1_is_type = if let Expr::Literal(Literal::Ident(ref ident)) = left {
            let s: &str = ident.as_ref();
            s == "make" || s == "new"
        } else {
            false
        };

        if arg1_is_type {
            ty = Some(p.parse_type()?);
        }

        while !p.lexer.match_token(TokenKind::RParen)? {
            args.push(p.parse_expr_until(BP_COMMA)?);
            if !p.lexer.match_token(TokenKind::Comma)? {
                p.lexer.expect_token(TokenKind::RParen)?;
                break;
            }

            if p.lexer.match_token(TokenKind::Ellipsis)? {
                p.lexer.expect_token(TokenKind::RParen)?;
                ellipsis = true;
                break;
            }
        }
    };

    Ok(Expr::Call {
        left: P(left),
        exprs: args.into(),
        ty,
        ellipsis
    })
}

impl<'a> Parser<'a> {
    fn parse_null(&mut self, token: Token) -> CompileResult<Result<Expr, Token>> {
        use self::TokenKind::*;

        let (nud, bp): (NullDenotation, i32) = match token.kind {
            Ident(_) | Integer(_) | StrLit(_) => {
                (null_constant, -1)
            }
            LParen => {
                (null_paren, BP_PARENS)
            }
            Plus | Minus | LogNot | Caret | Star | And | SendReceive => {
                (null_prefix_op, BP_UNARY)
            }
            _ => {
                return Ok(Err(token));
            }
        };

        nud(self, token, bp).map(|e| Ok(e))
    }

    fn parse_left(&mut self, min_rbp: i32, node: Expr) -> CompileResult<Expr> {
        use self::TokenKind::*;

        let (led, bp): (LeftDenotation, i32) = {
            let token = if let Some(token) = self.lexer.peek()? {
                token
            } else {
                return Ok(node); // No tokens left, terminate expression.
            };

            match token.kind {
                LParen => (left_func_call, BP_ACCESSOR),
                LBracket => (left_index, BP_ACCESSOR),
                Dot => (left_selector, BP_ACCESSOR),

                LogOr => (left_binary_op, BP_LOGOR),
                LogAnd => (left_binary_op, BP_LOGAND),
                Equals | NotEqual | Less | LessOrEqual | Greater | GreaterOrEqual => (left_binary_op, BP_COMPARISON),
                Plus | Minus | Or | Caret => (left_binary_op, BP_ADD),
                Star | Slash | Percent | LShift | RShift | And | AndNot => (left_binary_op, BP_MUL),

                _ => {
                    // This token cannot form part of the expression, so terminate
                    // TODO: Are there any tokens that shouldn't terminate an expression error free?
                    return Ok(node);
                }
            }
        };

        let (lbp, rbp) = (bp, bp);

        if min_rbp >= lbp {
            // We've reached something that binds tighter than us, so terminate this iteration.
            return Ok(node);
        }

        // The use of peek above has already ensured there's a token, so unwrap here.
        let token = self.lexer.next()?.unwrap();

        let node = led(self, token, node, rbp)?;

        // Not done
        self.parse_left(min_rbp, node)
    }

    fn parse_expr_until(&mut self, min_rbp: i32) -> CompileResult<Expr> {
        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            let span = Span::new(self.lexer.offset(), self.lexer.offset());
            return err!(span, "expected expression, found end of file");
        };

        let node = match self.parse_null(token)? {
            Ok(node) => node,
            Err(token) => return err!(token, "expected expression, found {:#?}", token),
        };

        self.parse_left(min_rbp, node)
    }

    fn parse_opt_expr_until(&mut self, min_rbp: i32) -> CompileResult<Option<Expr>> {
        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            let span = Span::new(self.lexer.offset(), self.lexer.offset());
            return err!(span, "expected expression, found end of file");
        };

        let node = if let Ok(node) = self.parse_null(token)? {
            node
        } else {
            return Ok(None);
        };

        self.parse_left(min_rbp, node).map(|e| Some(e))
    }

    pub fn parse_expr(&mut self) -> CompileResult<Expr> {
        self.parse_expr_until(0)
    }

    pub fn parse_opt_expr(&mut self) -> CompileResult<Option<Expr>> {
        self.parse_opt_expr_until(0)
    }
}



//
// Statement parsing
//
impl<'src> Parser<'src> {
    fn expect_terminal(&mut self) -> CompileResult<()> {
        self.lexer.expect_token(TokenKind::Semicolon)
    }

    fn expect_terminal_or_end_scope(&mut self) -> CompileResult<bool> {
        let token = self.lexer.next()?;
        match token.map(|t| t.kind) {
            Some(TokenKind::Semicolon) => {

            }
            Some(TokenKind::RBrace) | Some(TokenKind::RParen) => {

            }
            Some(other) => {

            }
            None => {

            }
        }
        unimplemented!()
    }

    fn parse_block(&mut self) -> CompileResult<Block> {
        let mut stmts = Vec::new();
        while !self.lexer.match_token(TokenKind::RBrace)? {
            stmts.push(*self.parse_stmt()?);

            // Semicolons can be omitted when a '}' follows, allowing succinct syntax such as:
            // if cond { do_thing() }
            if !self.lexer.match_token(TokenKind::Semicolon)? {
                if self.lexer.match_token(TokenKind::RBrace)? {
                    break;
                } else {
                    let token = self.lexer.next()?;
                    return err!(token, "expected ';' or '}}', got {:#?}", token);
                }
            }
        }
        Ok(List::from(stmts))
    }

    fn parse_decl(&mut self) -> CompileResult<Declaration> {
        unimplemented!()
    }

    fn parse_if_stmt(&mut self) -> CompileResult<IfStmt> {
        let pre_stmt = self.parse_opt_simple_stmt()?;
        if pre_stmt.is_some() {
            self.expect_terminal()?;
        }
        let cond = P(self.parse_expr()?);
        self.lexer.expect_token(TokenKind::LBrace)?;
        let then = self.parse_block()?;

        let els = if self.lexer.match_keyword(Keyword::Else)? {
            if self.lexer.match_keyword(Keyword::If)? {
                IfStmtTail::ElseIf(P(self.parse_if_stmt()?))
            } else {
                self.lexer.expect_token(TokenKind::LBrace)?;
                IfStmtTail::Block(self.parse_block()?)
            }
        } else {
            IfStmtTail::None
        };

        Ok(IfStmt { pre_stmt, cond, then, els })
    }

    fn parse_for_stmt(&mut self) -> CompileResult<ForStmt> {
        unimplemented!()
    }

    fn parse_stmt(&mut self) -> CompileResult<P<Stmt>> {
        use self::Keyword::*;
        use self::TokenKind::Keyword;

        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            return err!(Span::INVALID, "expected statement, got end of file");
        };

        let stmt = match token.kind {
            Keyword(Var) | Keyword(Type) | Keyword(Const) => {
                Stmt::Declaration(self.parse_decl()?)
            }
            Keyword(Go) => {
                Stmt::Go(P(self.parse_expr()?))
            }
            Keyword(Return) => {
                let mut params = Vec::new();
                while let Some(expr) = self.parse_opt_expr()? {
                    params.push(expr);
                    if !self.lexer.match_token(TokenKind::Comma)? {
                        break;
                    }
                }
                Stmt::Return(params.into())
            }
            Keyword(Break) => {
                Stmt::Break(self.lexer.match_ident()?)
            }
            Keyword(Continue) => {
                Stmt::Continue(self.lexer.match_ident()?)
            }
            Keyword(Goto) => {
                Stmt::Goto(self.lexer.expect_ident()?)
            }
            Keyword(Fallthrough) => {
                Stmt::Fallthrough
            }
            Keyword(If) => {
                Stmt::If(self.parse_if_stmt()?)
            }
            Keyword(Switch) => {
                unimplemented!()
            }
            Keyword(Select) => {
                unimplemented!()
            }
            Keyword(For) => {
                Stmt::For(self.parse_for_stmt()?)
            }
            Keyword(Defer) => {
                Stmt::Defer(P(self.parse_expr()?))
            }
            TokenKind::LBrace => {
                Stmt::Block(self.parse_block()?)
            }
            _ => unimplemented!("{:#?}", token)
        };

        Ok(P(stmt))
    }

    fn parse_opt_simple_stmt(&mut self) -> CompileResult<Option<SimpleStmt>> {
        let expr = if let Some(expr) = self.parse_opt_expr()? {
            expr
        } else {
            return Ok(None);
        };

        let checkpoint = self.lexer.checkpoint();
        let next_token = self.lexer.next()?;

        let simple_stmt = match next_token.map(|t| t.kind) {
            None => {
                SimpleStmt::Expr(P(expr))
            }
            Some(TokenKind::SendReceive) => {
                let channel = P(expr);
                let expr = P(self.parse_expr()?);
                SimpleStmt::Send { channel, expr }
            }
            Some(TokenKind::Increment) => {
                SimpleStmt::Increment(P(expr))
            }
            Some(TokenKind::Decrement) => {
                SimpleStmt::Decrement(P(expr))
            }
            Some(TokenKind::Assign(_)) | Some(TokenKind::ColonEq) => {
                // Replay the assignment operator
                self.lexer.backtrack(checkpoint);
                self.parse_assignment(vec![expr])?
            }
            Some(TokenKind::Comma) => {
                let mut left = self.parse_comma_separated_list(|p| p.parse_expr())?;
                left.insert(0, expr);

                self.parse_assignment(left)?
            }
            _ => return Ok(None)
        };

        Ok(Some(simple_stmt))
    }

    fn parse_assignment(&mut self, left: Vec<Expr>) -> CompileResult<SimpleStmt> {
        match self.lexer.next()? {
            Some(Token { kind: TokenKind::Assign(op), .. }) => {
                let right = self.parse_comma_separated_list(|p| p.parse_expr())?;
                if left.len() != right.len() {
                    let span = Span::new(self.lexer.offset(), self.lexer.offset());
                    return err!(span, "assignment count mismatch: {} = {}",
                        left.len(), right.len())
                }
                Ok(SimpleStmt::Assignment { left: left.into(), right: right.into(), op })
            }
            Some(Token { kind: TokenKind::ColonEq, .. }) => {
                let exprs = self.parse_comma_separated_list(|p| p.parse_expr())?.into();
                let idents = left.into_iter().map(|e| {
                    if let Expr::Literal(Literal::Ident(ident)) = e {
                        Ok(ident)
                    } else {
                        // TODO: Span
                        err!(Span::INVALID, "non-name {:#?} on left side of :=", e)
                    }
                }).collect::<CompileResult<Vec<Ident>>>()?.into();
                Ok(SimpleStmt::ShortVarDecl { idents, exprs })
            }
            None => {
                return err!(Span::INVALID, "expected assignment, got end of file");
            }
            other => {
                return err!(Span::INVALID, "expected assignment, got {:#?}", other)
            }
        }
    }

    fn parse_comma_separated_list<R, F>(&mut self, f: F) -> CompileResult<Vec<R>>
        where F: Fn(&mut Parser) -> CompileResult<R>
    {
        let mut result = vec![f(self)?];

        while self.lexer.match_token(TokenKind::Comma)? {
            result.push(f(self)?);
        }

        Ok(result.into())
    }
}