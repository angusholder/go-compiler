use std::collections::VecDeque;
use std::mem;

use ast::*;
use lexer::{ Lexer, Token, TokenKind, Keyword };
use utils::ptr::{ List, P };
use utils::result::{ CompileResult, Span };

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    token: Token,
    lookahead_stack: VecDeque<Token>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Parser<'src> {
        Parser {
            lexer: Lexer::new(src),
            lookahead_stack: VecDeque::new(),
            token: Token {
                kind: TokenKind::Eof,
                span: Span::INVALID,
            }
        }
    }

    pub fn parse(&mut self) -> CompileResult<SourceFile> {
        self.expect_keyword(Keyword::Package)?;
        let package_name = self.expect_ident()?;
        self.expect_terminal()?;

        let mut imports: Vec<ImportSpec> = Vec::new();
        while self.expect_keyword(Keyword::Import)? {
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
    fn look_ahead<R, F>(&mut self, dist: usize, f: F) -> CompileResult<R>
        where F: FnOnce(&Token) -> R
    {
        if dist == 0 {
            Ok(f(&self.token))
        } else {
            let offset = dist - 1;
            while self.lookahead_stack.len() < offset {
                let token = self.lexer.next()?;
                self.lookahead_stack.push(token);
            }
            Ok(f(&self.lookahead_stack[offset]))
        }
    }

    fn bump(&mut self) -> CompileResult<Token> {
        let token = if let Some(token) = self.lookahead_stack.pop_front() {
            token
        } else {
            self.lexer.next()?
        };
        Ok(mem::replace(&mut self.token, token))
    }

    fn unbump(&mut self, token: Token) {
        let next = mem::replace(&mut self.token, token);
        self.lookahead_stack.push_front(next);
    }

    fn bump_if<R, F>(&mut self, f: F) -> CompileResult<Option<R>>
        where F: FnOnce(Token) -> Result<R, Token>
    {
        match f(self.bump()?) {
            Ok(r) => Ok(Some(r)),
            Err(tok) => {
                self.unbump(tok);
                Ok(None)
            }
        }
    }

    pub fn match_keyword(&mut self, keyword: Keyword) -> CompileResult<bool> {
        if self.token.kind == TokenKind::Keyword(keyword) {
            self.bump()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_token(&mut self, token_kind: TokenKind) -> CompileResult<bool> {
        if self.token.kind == token_kind {
            self.bump()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_ident(&mut self) -> CompileResult<Option<Ident>> {
        Ok(self.bump_if(|t| {
            if let TokenKind::Ident(ident) = t {
                Ok(ident)
            } else {
                Err(t)
            }
        }))
    }

    fn expect_token(&mut self, token_kind: TokenKind) -> CompileResult<()> {
        if self.token.kind == token_kind {
            self.bump()
        } else {
            err!(next, "expected token {:#?}, got {:#?}", token_kind, self.token)
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> CompileResult<()> {
        if self.token.kind == TokenKind::Keyword(keyword) {
            self.bump()
        } else {
            err!(next, "expected keyword {:#?}, got {:#?}", keyword, self.token)
        }
    }

    fn expect_ident(&mut self) -> CompileResult<Ident> {
        self.match_ident()?.ok_or(format!("expected identifier, got {:#?}", self.token))
    }

    fn expect_string_lit(&mut self) -> CompileResult<String> {
        self.bump_if(|t| {
            if let TokenKind::Ident(ident) = t {
                Ok(ident)
            } else {
                Err(t)
            }
        }).ok_or(format!("expected string literal, got {:#?}", self.token))
    }
}

impl<'src> Parser<'src> {
    fn parse_import_decl(&mut self) -> CompileResult<ImportDecl> {
        let mut specs = Vec::new();
        if self.expect_token(TokenKind::LParen)? {
            while !self.expect_token(TokenKind::RParen)? {
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
        if self.expect_token(TokenKind::Dot)? {
            alias = ImportAlias::Splat;
        } else if let Some(ident) = self.expect_ident()? {
            alias = ImportAlias::Name(ident);
        } else {
            alias = ImportAlias::None;
        }

        let path = self.expect_string_lit()?;

        Ok(ImportSpec { alias, path })
    }

    fn parse_top_level_decl(&mut self) -> CompileResult<Option<TopLevelDecl>> {
        use self::Keyword::*;

        match self.token.kind {
            TokenKind::Keyword(Func) => {
                self.bump()?;
                Ok(Some(TopLevelDecl::Function(self.parse_func()?)))
            }
            TokenKind::Keyword(Const) | TokenKind::Keyword(Type) | TokenKind::Keyword(Var) => {
                unimplemented!()
            }
            TokenKind::Eof => Ok(None),
            _ => {
                err!(token, "expected top level declaration (const/type/var/func), found {:#?}",
                    token)
            }
        }
    }

    fn parse_parameter_decl(&mut self) -> CompileResult<Option<ParameterDecl>> {
        let idents: Option<List<Ident>>;

        if let Some(ident) = self.expect_ident()? {
            let mut list = vec![ident];
            while self.expect_token(TokenKind::Comma)? {
                list.push(self.expect_ident()?);
            }
            idents = Some(list.into());
        } else {
            idents = None;
        }

        let ellipsis = self.expect_token(TokenKind::Ellipsis)?;
        let ty = self.parse_opt_type()?;

        if let Some(ty) = ty {
            Ok(Some(ParameterDecl {
                idents: idents.into(),
                ellipsis,
                ty,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_func(&mut self) -> CompileResult<FunctionDecl> {
        let name = self.expect_ident()?;
        self.expect_token(TokenKind::LParen)?;
        let sig = self.parse_signature()?;
        let body = if self.expect_token(TokenKind::LBrace)? {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(FunctionDecl { name, sig, body })
    }

    fn parse_parameter_list(&mut self) -> CompileResult<List<ParameterDecl>> {
        let mut params = Vec::new();

        if !self.expect_token(TokenKind::RParen)? {
            while let Some(param) = self.parse_parameter_decl()? {
                params.push(param);
                if !self.expect_token(TokenKind::Comma)? {
                    self.expect_token(TokenKind::RParen)?;
                    break;
                }

                if self.expect_token(TokenKind::RParen)? {
                    break;
                }
            }
        }

        Ok(params.into())
    }

    fn parse_signature(&mut self) -> CompileResult<Signature> {
        let params = self.parse_parameter_list()?;

        let result = if self.expect_token(TokenKind::LParen)? {
            FuncResult::Many(self.parse_parameter_list()?)
        } else if let Some(ty) = self.parse_opt_type()? {
            FuncResult::One(ty)
        } else {
            FuncResult::None
        };

        Ok(Signature { params, result })
    }

    fn parse_opt_type(&mut self) -> CompileResult<Option<P<Type>>> {
        let token = self.bump()?;
        let ty = match token.kind {
            TokenKind::Ident(package_or_ident) => {
                if self.expect_token(TokenKind::Dot)? {
                    let ident = self.expect_ident()?;
                    P(Type::TypeName { package: Some(package_or_ident), ident, })
                } else {
                    P(Type::TypeName { package: None, ident: package_or_ident })
                }
            }
            TokenKind::LParen => {
                let ty = self.parse_type()?;
                self.expect_token(TokenKind::RParen)?;
                ty
            }
            TokenKind::LBracket => {
                if self.expect_token(TokenKind::RBracket)? {
                    P(Type::TypeLit(TypeLit::SliceType(self.parse_type()?)))
                } else {
                    let length = P(self.parse_expr()?);
                    self.expect_token(TokenKind::RBracket)?;
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
                self.expect_token(TokenKind::LBracket)?;
                let key_ty = self.parse_type()?;
                self.expect_token(TokenKind::RBracket)?;
                let elem_ty = self.parse_type()?;
                P(Type::TypeLit(TypeLit::MapType { key_ty, elem_ty }))
            }
            token @ TokenKind::Keyword(Keyword::Chan) | token @ TokenKind::SendReceive => {
                let direction: ChannelDirection;
                if token == TokenKind::SendReceive {
                    self.expect_keyword(Keyword::Chan)?;
                    direction = ChannelDirection::Receive;
                } else if self.expect_token(TokenKind::SendReceive)? {
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
                self.unbump(token);
                return Ok(None);
            }
        };
        Ok(Some(ty))
    }

    fn parse_type(&mut self) -> CompileResult<P<Type>> {
        if let Some(ty) = self.parse_opt_type()? {
            Ok(ty)
        } else {
            err!(self.token, "expected type, got {:#?}", self.token)
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
    fn parse_null(&mut self) -> CompileResult<Option<Expr>> {
        use self::TokenKind::*;

        let (nud, bp): (NullDenotation, i32) = match self.token.kind {
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
                return Ok(None);
            }
        };

        nud(self, token, bp).map(|e| Ok(e))
    }

    fn parse_left(&mut self, min_rbp: i32, node: Expr) -> CompileResult<Expr> {
        use self::TokenKind::*;

        let (led, bp): (LeftDenotation, i32) = match self.token.kind {
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
        };

        let (lbp, rbp) = (bp, bp);

        if min_rbp >= lbp {
            // We've reached something that binds tighter than us, so terminate this iteration.
            Ok(node)
        } else {
            // Grab the current token and jump to the next
            let token = self.bump()?;

            let node = led(self, token, node, rbp)?;

            // Not done
            self.parse_left(min_rbp, node)
        }
    }

    fn parse_expr_until(&mut self, min_rbp: i32) -> CompileResult<Expr> {
        let node = if let Some(node) = self.parse_null()? {
            node
        } else {
            return err!(self.token, "expected expression, found {:#?}", self.token)
        };

        self.parse_left(min_rbp, node)
    }

    fn parse_opt_expr_until(&mut self, min_rbp: i32) -> CompileResult<Option<Expr>> {
        let node = if let Ok(node) = self.parse_null()? {
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
        self.expect_token(TokenKind::Semicolon)
    }

    fn parse_block(&mut self) -> CompileResult<Block> {
        let mut stmts = Vec::new();
        while !self.expect_token(TokenKind::RBrace)? {
            stmts.push(self.parse_stmt()?);

            // Semicolons can be omitted when a '}' follows, allowing succinct syntax such as:
            // if cond { do_thing() }
            if !self.expect_token(TokenKind::Semicolon)? {
                if self.expect_token(TokenKind::RBrace)? {
                    break;
                } else {
                    return err!(self.token, "expected ';' or '}}', got {:#?}", self.token);
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
        self.expect_token(TokenKind::LBrace)?;
        let then = self.parse_block()?;

        let els = if self.expect_keyword(Keyword::Else)? {
            if self.expect_keyword(Keyword::If)? {
                IfStmtTail::ElseIf(P(self.parse_if_stmt()?))
            } else {
                self.expect_token(TokenKind::LBrace)?;
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

    fn parse_stmt(&mut self) -> CompileResult<Stmt> {
        use self::Keyword::*;
        use self::TokenKind::Keyword;

        let checkpoint = self.lexer.checkpoint();

        let stmt = match self.token.kind {
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
                    if !self.expect_token(TokenKind::Comma)? {
                        break;
                    }
                }
                Stmt::Return(params.into())
            }
            Keyword(Break) => {
                Stmt::Break(self.expect_ident()?)
            }
            Keyword(Continue) => {
                Stmt::Continue(self.expect_ident()?)
            }
            Keyword(Goto) => {
                Stmt::Goto(self.expect_ident()?)
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
            _ => {
                self.unbump(token);
                let simple = if let Some(simple) = self.parse_opt_simple_stmt()? {
                    simple
                } else {
                    return err!(self.token, "expected statement, got {:#?}", self.token);
                };
                Stmt::Simple(simple)
            }
        };

        Ok(stmt)
    }

    fn parse_opt_simple_stmt(&mut self) -> CompileResult<Option<SimpleStmt>> {
        let before_expr_checkpoint = self.lexer.checkpoint();

        let expr = if let Some(expr) = self.parse_opt_expr()? {
            expr
        } else {
            return Ok(None);
        };

        let after_expr_checkpoint = self.lexer.checkpoint();

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
                self.lexer.backtrack(after_expr_checkpoint);
                self.parse_assignment(vec![expr])?
            }
            Some(TokenKind::Comma) => {
                let mut left = self.parse_comma_separated_list(|p| p.parse_expr())?;
                left.insert(0, expr);

                self.parse_assignment(left)?
            }
            Some(TokenKind::Semicolon) | Some(TokenKind::RBrace) => {
                self.lexer.backtrack(after_expr_checkpoint);
                SimpleStmt::Expr(P(expr))
            }
            _ => {
                self.lexer.backtrack(before_expr_checkpoint);
                return Ok(None)
            }
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

        while self.expect_token(TokenKind::Comma)? {
            result.push(f(self)?);
        }

        Ok(result.into())
    }
}