use ast::*;
use lexer::{ Lexer, Token, TokenKind, Keyword };
use ptr::{ List, P };
use result::CompileResult;

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
        while let Some(decl) = self.parse_decl()? {
            decls.push(decl);
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
                self.lexer.expect_token(TokenKind::Semicolon)?;
            }
        } else {
            specs.push(self.parse_import_spec()?);
            self.lexer.expect_token(TokenKind::Semicolon)?;
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

    fn parse_decl(&mut self) -> CompileResult<Option<TopLevelDecl>> {
        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            return Ok(None);
        };

        match token.kind {
            TokenKind::Keyword(Keyword::Func) => {
                Ok(Some(TopLevelDecl::Function(self.parse_func()?)))
            }
            _ => {
                err!(token, "Expected top level declaration (const/type/var/func), found {:#?}",
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

    fn parse_opt_type(&mut self) -> CompileResult<Option<P<Type>>> {
        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            return Ok(None);
        };
        let ty: P<Type> = match token.kind {
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
                    let length = self.parse_expr()?;
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
                self.lexer.unget(Some(Token { kind: other }));
                return Ok(None);
            }
        };
        Ok(Some(ty))
    }

    fn parse_type(&mut self) -> CompileResult<P<Type>> {
        if let Some(ty) = self.parse_opt_type()? {
            Ok(ty)
        } else {
            let next = self.lexer.next()?;
            err!(next, "expected type, got {:#?}", next)
        }
    }

    fn parse_block(&mut self) -> CompileResult<Block> {
        let mut stmts = Vec::new();
        while !self.lexer.match_token(TokenKind::RBrace)? {
            stmts.push(*self.parse_stmt()?);
        }
        Ok(List::from(stmts))
    }

    fn parse_stmt(&mut self) -> CompileResult<P<Stmt>> {
        unimplemented!()
    }

    fn parse_expr(&mut self) -> CompileResult<P<Expr>> {
        unimplemented!()
    }
}