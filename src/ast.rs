use lexer::AssignOp;
use utils::ptr::{ P, List };
use utils::result::Span;
use utils::result::HasSpan;
use types::{ self, TypeRef };
use utils::intern::Atom;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Plus,    // +
    Minus,   // -
    LogNot,  // !
    Not,     // ^
    Deref,   // *
    AddrOf,  // &
    Receive, // <-
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinaryOp {
    LogOr,
    LogAnd,

    // Relational op
    Equals,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,

    // Add op
    Add,
    Sub,
    Or,
    Xor,

    // Mul op
    Mul,
    Div,
    Remainder,
    LShift,
    RShift,
    And,
    AndNot,
}

#[derive(Debug)]
pub enum Literal {
    Int(u64),
    // TODO: Float, Imag, Rune
    String(String),
    Ident(Atom),
    // TODO: CompositeLit
    Function {
        sig: Signature,
        body: Block,
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub ty: TypeRef,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Expr {
        let ty = types::UNRESOLVED;
        Expr { kind, span, ty }
    }
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal),
    Unary {
        child: P<Expr>,
        op: UnaryOp,
    },
    Binary {
        left: P<Expr>,
        right: P<Expr>,
        op: BinaryOp,
    },
    Conversion {
        ty: P<Type>,
        args: List<Expr>,
    },
    Selector {
        left: P<Expr>,
        field_name: Atom,
    },
    Index {
        left: P<Expr>,
        right: P<Expr>,
    },
    Slice {
        left: P<Expr>,
        start: Option<P<Expr>>,
        end: Option<P<Expr>>,
        // Invariant: if max.is_some() then end.is_some()
        max: Option<P<Expr>>,
    },
    TypeAssertion {
        left: P<Expr>,
        ty: P<Type>,
    },
    Call {
        left: P<Expr>,
        // For make(T, n) and new(T)
        ty: Option<P<Type>>,
        exprs: List<Expr>,
        ellipsis: bool,
    },
}

pub type Label = Atom;
pub type Block = List<Stmt>;

#[derive(Debug)]
pub enum IfStmtTail {
    None,
    ElseIf(P<IfStmt>),
    Block(Block),
}

#[derive(Debug)]
pub struct IfStmt {
    pub pre_stmt: Option<SimpleStmt>,
    pub cond: P<Expr>,
    pub then: Block,
    pub els: IfStmtTail,
}

#[derive(Debug)]
pub enum RangeClauseLeft {
    Exprs(List<Expr>),
    Idents(List<Atom>),
}

#[derive(Debug)]
pub enum ForStmtHeader {
    Always,
    Condition(P<Expr>),
    ForClause {
        init_stmt: Option<SimpleStmt>,
        cond: Option<P<Expr>>,
        post_stmt: Option<SimpleStmt>,
    },
    RangeClause(RangeClause),
}

#[derive(Debug)]
pub struct RangeClause {
    pub left: RangeClauseLeft,
    pub right: P<Expr>,
}

#[derive(Debug)]
pub struct ForStmt {
    pub header: ForStmtHeader,
    pub body: Block,
}

#[derive(Debug)]
pub enum SimpleStmt {
    Expr(P<Expr>),
    Send {
        channel: P<Expr>,
        expr: P<Expr>,
    },
    Increment(P<Expr>),
    Decrement(P<Expr>),
    Assignment {
        left: List<Expr>,
        right: List<Expr>,
        op: AssignOp,
    },
    ShortVarDecl {
        idents: List<Atom>,
        exprs: List<Expr>,
    },
    RangeClause(RangeClause),
}

#[derive(Debug)]
pub struct ConstSpec {
    pub idents: List<Atom>,
    pub ty: Option<P<Type>>,
    pub exprs: Option<List<Expr>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TypeSpecKind {
    AliasDecl,
    TypeDef,
}

#[derive(Debug)]
pub struct TypeSpec {
    pub kind: TypeSpecKind,
    pub ident: Atom,
    pub ty: P<Type>,
}

/// Invariant: either `ty` or `exprs` must be `Some`
#[derive(Debug)]
pub struct VarSpec {
    pub idents: List<Atom>,
    pub ty: Option<P<Type>>,
    pub exprs: Option<List<Expr>>,
}

#[derive(Debug)]
pub enum Declaration {
    Const(List<ConstSpec>),
    Type(List<TypeSpec>),
    Var(List<VarSpec>),
}

#[derive(Debug)]
pub enum Stmt {
    Declaration(Declaration),
    Labeled {
        label: Label,
        stmt: P<Stmt>,
    },
    Simple(SimpleStmt),
    Go(P<Expr>),
    Return(List<Expr>),
    Break(Option<Label>),
    Continue(Option<Label>),
    Goto(Label),
    Fallthrough,
    Block(Block),
    If(IfStmt),
    // TODO: SwitchStmt, SelectStmt
    For(ForStmt),
    Defer(P<Expr>),
}

#[derive(Debug)]
pub enum Type {
    TypeName {
        ident: Atom,
        package: Option<Atom>,
    },
    TypeLit(TypeLit),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ChannelDirection {
    Send,
    Receive,
    BiDirectional,
}

#[derive(Debug)]
pub enum TypeLit {
    ArrayType {
        length: P<Expr>,
        elem_ty: P<Type>,
    },
    // TODO: StructType
    PointerType(P<Type>),
    FunctionType(Signature),
    // TODO: InterfaceType
    SliceType(P<Type>),
    MapType {
        key_ty: P<Type>,
        elem_ty: P<Type>,
    },
    ChannelType {
        direction: ChannelDirection,
        elem_ty: P<Type>,
    }
}

#[derive(Debug)]
pub enum ImportAlias {
    None,
    Splat,
    Name(Atom),
}

#[derive(Debug)]
pub struct ImportSpec {
    pub alias: ImportAlias,
    pub path: String,
}

#[derive(Debug)]
pub struct ParameterDecl {
    pub idents: Option<List<Atom>>,
    pub ellipsis: bool,
    pub ty: P<Type>,
}

#[derive(Debug)]
pub enum FuncResult {
    None,
    One(P<Type>),
    Many(List<ParameterDecl>),
}

#[derive(Debug)]
pub struct Signature {
    pub params: List<ParameterDecl>,
    pub result: FuncResult,
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Atom,
    pub sig: Signature,
    pub body: Option<Block>,
}

#[derive(Debug)]
pub struct MethodDecl {
    // Invariants: must contain a single, non-variadic parameter with type of the form T or *T
    pub receiver: ParameterDecl,
    pub name: Atom,
    pub sig: Signature,
    pub body: Option<Block>,
}

#[derive(Debug)]
pub enum TopLevelDecl {
    Declaration(Declaration),
    Function(FunctionDecl),
    Method(MethodDecl),
}

#[derive(Debug)]
pub struct ImportDecl {
    pub decls: List<ImportSpec>,
}

#[derive(Debug)]
pub struct SourceFile {
    pub package_name: Atom,

    // Flattened list of imports
    pub imports: List<ImportSpec>,

    pub decls: List<TopLevelDecl>,
}