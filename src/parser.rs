use std::collections::VecDeque;
use std::mem;

use ast::*;
use lexer::{ Lexer, Token, TokenKind, Keyword, AssignOp };
use utils::ptr::{ List, P };
use utils::result::{ CompileResult, Span, HasSpan };
use utils::id::Id;
use utils::intern::Atom;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    token: Token,
    lookahead_stack: VecDeque<Token>,
    prev_span: Span,
    expr_id: usize,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> CompileResult<Parser<'src>> {
        let mut parser = Parser {
            lexer: Lexer::new(src),
            lookahead_stack: VecDeque::new(),
            token: Token {
                kind: TokenKind::Eof,
                span: Span::INVALID,
            },
            prev_span: Span::INVALID,
            expr_id: 0,
        };
        parser.bump()?;
        Ok(parser)
    }

    pub fn parse(&mut self) -> CompileResult<SourceFile> {
        self.expect_keyword(Keyword::Package)?;
        let package_name = self.expect_ident()?;
        self.expect_terminal()?;

        let mut imports: Vec<ImportSpec> = Vec::new();
        while self.match_keyword(Keyword::Import)? {
            imports.extend(self.parse_import_decl()?.decls);
        }

        let mut decls: Vec<TopLevelDecl> = Vec::new();
        while let Some(decl) = self.parse_top_level_decl()? {
            decls.push(decl);
        }

        if self.token.kind != TokenKind::Eof {
            return err!(self.token, "expected top level declaration or end of file, got {}",
                self.token);
        }

        Ok(SourceFile {
            package_name,
            imports: imports.into(),
            decls: decls.into(),
        })
    }
}

impl<'src> Parser<'src> {
    fn bump(&mut self) -> CompileResult<Token> {
        let token = if let Some(token) = self.lookahead_stack.pop_front() {
            token
        } else {
            self.lexer.next()?
        };
        self.prev_span = token.span;
        Ok(mem::replace(&mut self.token, token))
    }

    fn unbump(&mut self, token: Token) {
        self.prev_span = Span::INVALID;
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

    fn match_ident(&mut self) -> CompileResult<Option<Atom>> {
        if let TokenKind::Ident(ident) = self.token.kind {
            self.bump()?;
            Ok(Some(ident))
        } else {
            Ok(None)
        }
    }

    fn expect_token(&mut self, token_kind: TokenKind) -> CompileResult<()> {
        if self.token.kind == token_kind {
            self.bump()?;
            Ok(())
        } else {
            err!(self.token, "expected token {:#?}, got {}", token_kind, self.token)
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> CompileResult<()> {
        if self.token.kind == TokenKind::Keyword(keyword) {
            self.bump()?;
            Ok(())
        } else {
            err!(self.token, "expected keyword {:#?}, got {}", keyword, self.token)
        }
    }

    fn expect_ident(&mut self) -> CompileResult<Atom> {
        if let Some(ident) = self.match_ident()? {
            Ok(ident)
        } else {
            err!(self.token, "expected identifier, got {}", self.token)
        }
    }

    fn expect_string_lit(&mut self) -> CompileResult<String> {
        let token = self.bump()?;
        if let TokenKind::StrLit(string) = token.kind {
            Ok(string)
        } else {
            self.unbump(token);
            err!(self.token, "expected string literal, got {}", self.token)
        }
    }

    fn span_from<T: HasSpan>(&self, has_span: &T) -> Span {
        Span::between(has_span.span(), self.prev_span)
    }

    fn make_expr(&mut self, kind: ExprKind, span: Span) -> Expr {
        let id = ExprId::from_usize(self.expr_id);
        self.expr_id += 1;
        Expr::new(kind, span, id)
    }
}

impl<'src> Parser<'src> {
    fn parse_import_decl(&mut self) -> CompileResult<ImportDecl> {
        let decls = self.parse_decl_body(Parser::parse_import_spec)?;
        self.expect_terminal()?;
        Ok(ImportDecl { decls })
    }

    fn parse_import_spec(&mut self) -> CompileResult<ImportSpec> {
        let alias: ImportAlias;
        if self.match_token(TokenKind::Dot)? {
            alias = ImportAlias::Splat;
        } else if let Some(ident) = self.match_ident()? {
            alias = ImportAlias::Name(ident);
        } else {
            alias = ImportAlias::None;
        }

        let path = self.expect_string_lit()?;

        Ok(ImportSpec { alias, path })
    }

    fn parse_top_level_decl(&mut self) -> CompileResult<Option<TopLevelDecl>> {
        use self::Keyword::*;

        let decl = match self.token.kind {
            TokenKind::Keyword(Func) => {
                self.bump()?;
                TopLevelDecl::Function(self.parse_func()?)
            }
            TokenKind::Keyword(Const) | TokenKind::Keyword(Type) | TokenKind::Keyword(Var) => {
                TopLevelDecl::Declaration(self.parse_decl()?)
            }
            TokenKind::Eof => return Ok(None),
            _ => {
                return err!(self.token, "expected top level declaration (const/type/var/func), found {}",
                    self.token);
            }
        };
        self.expect_terminal()?;
        Ok(Some(decl))
    }

    fn parse_parameter_decl(&mut self) -> CompileResult<Option<ParameterDecl>> {
        let idents: Option<List<Atom>>;

        if let Some(ident) = self.match_ident()? {
            let mut list = vec![ident];
            while self.match_token(TokenKind::Comma)? {
                list.push(self.expect_ident()?);
            }
            idents = Some(list.into());
        } else {
            idents = None;
        }

        let ellipsis = self.match_token(TokenKind::Ellipsis)?;
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
        let body = if self.token.kind == TokenKind::LBrace {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(FunctionDecl { name, sig, body })
    }

    fn parse_parameter_list(&mut self) -> CompileResult<List<ParameterDecl>> {
        let mut params = Vec::new();

        if !self.match_token(TokenKind::RParen)? {
            while let Some(param) = self.parse_parameter_decl()? {
                params.push(param);
                if !self.match_token(TokenKind::Comma)? {
                    self.expect_token(TokenKind::RParen)?;
                    break;
                }

                if self.match_token(TokenKind::RParen)? {
                    break;
                }
            }
        }

        Ok(params.into())
    }

    fn parse_signature(&mut self) -> CompileResult<Signature> {
        let params = self.parse_parameter_list()?;

        let result = if self.match_token(TokenKind::LParen)? {
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
                if self.match_token(TokenKind::Dot)? {
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
                if self.match_token(TokenKind::RBracket)? {
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
                } else if self.match_token(TokenKind::SendReceive)? {
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
            _ => {
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
            err!(self.token, "expected type, got {}", self.token)
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

fn null_constant(p: &mut Parser, token: Token, _bp: i32) -> CompileResult<Expr> {
    Ok(p.make_expr(ExprKind::Literal(match token.kind {
        TokenKind::Ident(i) => Literal::Ident(i),
        TokenKind::Integer(n) => Literal::Int(n),
        TokenKind::StrLit(s) => Literal::String(s),
        _ => unreachable!(),
    }), token.span))
}

fn null_paren(p: &mut Parser, _token: Token, bp: i32) -> CompileResult<Expr> {
    let result = p.parse_expr_until(bp)?;
    p.expect_token(TokenKind::RParen)?;
    Ok(result)
}

fn null_prefix_op(p: &mut Parser, token: Token, bp: i32) -> CompileResult<Expr> {
    let child = P(p.parse_expr_until(bp)?);
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

    let span = Span::between(token.span, child.span);
    Ok(p.make_expr(ExprKind::Unary { op, child }, span))
}

fn left_index(p: &mut Parser, token: Token, left: Expr, _rbp: i32) -> CompileResult<Expr> {
    let right = p.parse_opt_expr()?;
    if p.match_token(TokenKind::Colon)? {
        let start = right;
        let end = p.parse_opt_expr()?;
        let mut max: Option<Expr> = None;
        if p.match_token(TokenKind::Colon)? {
            max = p.parse_opt_expr()?;
        }
        p.expect_token(TokenKind::RBracket)?;

        let span = p.span_from(&left);
        Ok(p.make_expr(ExprKind::Slice {
            left: P(left),
            start: start.map(P),
            end: end.map(P),
            max: max.map(P)
        }, span))
    } else {
        if let Some(right) = right {
            p.expect_token(TokenKind::RBracket)?;
            let span = p.span_from(&left);
            Ok(p.make_expr(ExprKind::Index { left: P(left), right: P(right) }, span))
        } else {
            err!(token, "expected expression")
        }
    }
}

fn left_selector(p: &mut Parser, _token: Token, left: Expr, _rbp: i32) -> CompileResult<Expr> {
    if p.match_token(TokenKind::LParen)? {
        // Type Assertion
        let ty = p.parse_type()?;
        p.expect_token(TokenKind::RParen)?;
        let span = p.span_from(&left);
        Ok(p.make_expr(ExprKind::TypeAssertion { left: P(left), ty }, span))
    } else {
        // Field Access
        let field_name = p.expect_ident()?;
        let span = p.span_from(&left);
        Ok(p.make_expr(ExprKind::Selector { left: P(left), field_name }, span))
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

    let right = P(p.parse_expr_until(rbp)?);

    let span = Span::between(left.span, right.span);
    Ok(p.make_expr(ExprKind::Binary { op, left: P(left), right }, span))
}

fn left_func_call(p: &mut Parser, _token: Token, left: Expr, _rbp: i32) -> CompileResult<Expr> {
    let mut args = Vec::new();
    let mut ty: Option<P<Type>> = None;
    let mut ellipsis = false;

    if !p.match_token(TokenKind::RParen)? {
        let arg1_is_type = if let ExprKind::Literal(Literal::Ident(ref ident)) = left.kind {
            let s: &str = ident.as_ref();
            s == "make" || s == "new"
        } else {
            false
        };

        if arg1_is_type {
            ty = Some(p.parse_type()?);
        }

        while !p.match_token(TokenKind::RParen)? {
            args.push(p.parse_expr_until(BP_COMMA)?);
            if !p.match_token(TokenKind::Comma)? {
                p.expect_token(TokenKind::RParen)?;
                break;
            }

            if p.match_token(TokenKind::Ellipsis)? {
                p.expect_token(TokenKind::RParen)?;
                ellipsis = true;
                break;
            }
        }
    };

    let span = p.span_from(&left);
    Ok(p.make_expr(ExprKind::Call {
        left: P(left),
        exprs: args.into(),
        ty,
        ellipsis
    }, span))
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

        let token = self.bump()?;
        nud(self, token, bp).map(Some)
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
        let node = if let Some(node) = self.parse_null()? {
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
        self.expect_token(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while !self.match_token(TokenKind::RBrace)? {
            stmts.push(self.parse_stmt()?);

            // Semicolons can be omitted when a '}' follows, allowing succinct syntax such as:
            // if cond { do_thing() }
            if !self.match_token(TokenKind::Semicolon)? {
                if self.match_token(TokenKind::RBrace)? {
                    break;
                } else {
                    return err!(self.token, "expected ';' or '}}', got {}", self.token);
                }
            }
        }
        Ok(List::from(stmts))
    }

    fn parse_decl(&mut self) -> CompileResult<Declaration> {
        let token = self.bump()?;

        use self::Keyword::{ Var, Type, Const };
        use self::TokenKind::Keyword;
        let decl = match token.kind {
            Keyword(Var) => {
                Declaration::Var(self.parse_decl_body(Parser::parse_var_spec)?.into())
            }
            Keyword(Type) => {
                Declaration::Type(self.parse_decl_body(Parser::parse_type_spec)?.into())
            }
            Keyword(Const) => {
                Declaration::Const(self.parse_decl_body(Parser::parse_const_spec)?.into())
            }
            _ => {
                return err!(token, "expected declaration, got {}", token);
            }
        };
        Ok(decl)
    }

    fn parse_decl_body<R>(&mut self, f: fn(&mut Parser<'src>) -> CompileResult<R>) -> CompileResult<List<R>>
    {
        if self.match_token(TokenKind::LParen)? {
            let mut result = Vec::new();
            while !self.match_token(TokenKind::RParen)? {
                result.push(f(self)?);
                if !self.match_token(TokenKind::Semicolon)? {
                    self.expect_token(TokenKind::RParen)?;
                    break;
                }
            }
            Ok(result.into())
        } else {
            Ok(vec![f(self)?].into())
        }
    }

    fn parse_var_spec(&mut self) -> CompileResult<VarSpec> {
        let idents = self.parse_comma_separated_list(Parser::expect_ident)?.into();
        let ty = self.parse_opt_type()?;
        let exprs = if self.match_token(TokenKind::Assign(AssignOp::None))? {
            Some(self.parse_comma_separated_list(Parser::parse_expr)?.into())
        } else {
            None
        };

        Ok(VarSpec { idents, ty, exprs })
    }

    fn parse_type_spec(&mut self) -> CompileResult<TypeSpec> {
        let ident = self.expect_ident()?;
        let kind = if self.match_token(TokenKind::Assign(AssignOp::None))? {
            TypeSpecKind::AliasDecl
        } else {
            TypeSpecKind::TypeDef
        };
        let ty = self.parse_type()?;

        Ok(TypeSpec { ident, kind, ty })
    }

    fn parse_const_spec(&mut self) -> CompileResult<ConstSpec> {
        let idents = self.parse_comma_separated_list(Parser::expect_ident)?.into();
        let ty = self.parse_opt_type()?;
        let exprs = if self.match_token(TokenKind::Assign(AssignOp::None))? {
            Some(self.parse_comma_separated_list(Parser::parse_expr)?.into())
        } else {
            None
        };

        Ok(ConstSpec { idents, ty, exprs })
    }

    fn parse_if_stmt(&mut self) -> CompileResult<IfStmt> {
        let pre_stmt = self.parse_opt_simple_stmt()?;
        if pre_stmt.is_some() {
            self.expect_terminal()?;
        }
        let cond = P(self.parse_expr()?);
        let then = self.parse_block()?;

        let els = if self.match_keyword(Keyword::Else)? {
            if self.match_keyword(Keyword::If)? {
                IfStmtTail::ElseIf(P(self.parse_if_stmt()?))
            } else {
                IfStmtTail::Block(self.parse_block()?)
            }
        } else {
            IfStmtTail::None
        };

        Ok(IfStmt { pre_stmt, cond, then, els })
    }

    fn parse_for_stmt(&mut self) -> CompileResult<ForStmt> {
        if self.token.kind == TokenKind::LBrace {
            let header = ForStmtHeader::Always;
            let body = self.parse_block()?;
            return Ok(ForStmt { header, body });
        };

        let simple_stmt = self.parse_opt_simple_stmt_ext(true)?;

        let header: ForStmtHeader;
        if let Some(SimpleStmt::RangeClause(range_clause)) = simple_stmt {
            header = ForStmtHeader::RangeClause(range_clause);
        } else {
            match self.token.kind {
                TokenKind::LBrace => {
                    if let Some(SimpleStmt::Expr(expr)) = simple_stmt {
                        header = ForStmtHeader::Condition(expr);
                    } else {
                        return err!(Span::INVALID, "loop condition must be an expression, got {:#?}", simple_stmt);
                    }
                }
                TokenKind::Semicolon => {
                    self.bump()?;

                    let init_stmt = simple_stmt;
                    let cond = self.parse_opt_expr()?.map(P);
                    self.expect_token(TokenKind::Semicolon)?;
                    let post_stmt = self.parse_opt_simple_stmt()?;

                    header = ForStmtHeader::ForClause { init_stmt, cond, post_stmt };
                }
                _ => {
                    return err!(self.token, "expected valid for-loop header, got {:#?} \nfollowed by {}", simple_stmt, self.token);
                }
            }
        }

        let body = self.parse_block()?;

        Ok(ForStmt { header, body })
    }

    fn parse_stmt(&mut self) -> CompileResult<Stmt> {
        use self::Keyword::*;
        use self::TokenKind::Keyword;

        let token = self.bump()?;
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
                    if !self.match_token(TokenKind::Comma)? {
                        break;
                    }
                }
                Stmt::Return(params.into())
            }
            Keyword(Break) => {
                Stmt::Break(self.match_ident()?)
            }
            Keyword(Continue) => {
                Stmt::Continue(self.match_ident()?)
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
                self.unbump(token);
                Stmt::Block(self.parse_block()?)
            }
            _ => {
                self.unbump(token);
                if let Some(simple) = self.parse_opt_simple_stmt()? {
                    Stmt::Simple(simple)
                } else {
                    return err!(self.token, "expected statement, got {}", self.token);
                }
            }
        };

        Ok(stmt)
    }

    fn parse_opt_simple_stmt_ext(&mut self, within_for_stmt_header: bool) -> CompileResult<Option<SimpleStmt>> {
        let expr = if let Some(expr) = self.parse_opt_expr()? {
            expr
        } else {
            return Ok(None);
        };

        let token = self.bump()?;

        use self::TokenKind::*;
        let simple_stmt = match token.kind {
            SendReceive => {
                let channel = P(expr);
                let expr = P(self.parse_expr()?);
                SimpleStmt::Send { channel, expr }
            }
            Increment => {
                SimpleStmt::Increment(P(expr))
            }
            Decrement => {
                SimpleStmt::Decrement(P(expr))
            }
            Assign(_) | ColonEq => {
                // Replay the assignment operator
                self.unbump(token);

                self.parse_assignment(vec![expr], within_for_stmt_header)?
            }
            Comma => {
                let mut left = self.parse_comma_separated_list(Parser::parse_expr)?;
                left.insert(0, expr);

                self.parse_assignment(left, within_for_stmt_header)?
            }
            _ => {
                self.unbump(token);
                SimpleStmt::Expr(P(expr))
            }
        };

        Ok(Some(simple_stmt))
    }

    fn parse_opt_simple_stmt(&mut self) -> CompileResult<Option<SimpleStmt>> {
        self.parse_opt_simple_stmt_ext(false)
    }

    fn parse_assignment(&mut self, left: Vec<Expr>, within_for_stmt_header: bool) -> CompileResult<SimpleStmt> {
        let token = self.bump()?;
        match token.kind {
            TokenKind::Assign(op) => {
                match self.try_parse_range_clause(RangeClauseLeft::Exprs(left.into()), within_for_stmt_header)? {
                    Ok(stmt) => return Ok(stmt),
                    Err(RangeClauseLeft::Exprs(exprs)) => {
                        let right = self.parse_comma_separated_list(Parser::parse_expr)?;

                        if exprs.len() != right.len() {
                            err!(Span::INVALID, "assignment count mismatch: {} = {}", exprs.len(), right.len())
                        } else {
                            Ok(SimpleStmt::Assignment { left: exprs.into(), right: right.into(), op })
                        }
                    }
                    Err(RangeClauseLeft::Idents(_)) => unreachable!(),
                }

            }
            TokenKind::ColonEq => {
                let idents = left.into_iter().map(|e| {
                    if let ExprKind::Literal(Literal::Ident(ident)) = e.kind {
                        Ok(ident)
                    } else {
                        err!(e, "non-name {:#?} on left side of :=", e)
                    }
                }).collect::<CompileResult<Vec<Atom>>>()?.into();

                match self.try_parse_range_clause(RangeClauseLeft::Idents(idents), within_for_stmt_header)? {
                    Ok(stmt) => return Ok(stmt),
                    Err(RangeClauseLeft::Idents(idents)) => {
                        let exprs = self.parse_comma_separated_list(Parser::parse_expr)?.into();
                        Ok(SimpleStmt::ShortVarDecl { idents, exprs })
                    }
                    Err(RangeClauseLeft::Exprs(_)) => unreachable!(),
                }
            }
            _ => {
                return err!(token, "expected assignment, got {}", token)
            }
        }
    }

    fn try_parse_range_clause(&mut self, left: RangeClauseLeft, within_for_stmt_header: bool) -> CompileResult<Result<SimpleStmt, RangeClauseLeft>> {
        if self.token.kind == TokenKind::Keyword(Keyword::Range) {
            if within_for_stmt_header {
                self.bump()?;
                let expr = P(self.parse_expr()?);
                Ok(Ok(SimpleStmt::RangeClause(RangeClause { left, right: expr })))
            } else {
                err!(self.token, "expected expression, found `range`")
            }
        } else {
            Ok(Err(left))
        }
    }

    fn parse_comma_separated_list<R, F>(&mut self, f: F) -> CompileResult<Vec<R>>
        where F: Fn(&mut Parser<'src>) -> CompileResult<R>
    {
        let mut result = vec![f(self)?];

        while self.match_token(TokenKind::Comma)? {
            result.push(f(self)?);
        }

        Ok(result.into())
    }
}

pub fn parse(src: &str) -> CompileResult<SourceFile> {
    Parser::new(src)?.parse()
}