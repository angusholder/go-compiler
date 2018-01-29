use std::fmt::{ self, Display };

use token::AssignOp;
use utils::ptr::{ P, List };
use utils::result::Span;
use utils::result::HasSpan;
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
    // Logical
    LogOr,
    LogAnd,

    // Comparison
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Remainder,

    // Bitwise
    LShift,
    RShift,
    Or,
    Xor,
    And,
    AndNot,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BinaryOp::*;
        f.write_str(match *self {
            LogOr => "||",
            LogAnd => "&&",

            // Relational op
            Equal => "==",
            NotEqual => "!=",
            Less => "<",
            LessOrEqual => "<=",
            Greater => ">",
            GreaterOrEqual => ">=",

            // Add op
            Add => "+",
            Sub => "-",
            Or => "|",
            Xor => "^",

            // Mul op
            Mul => "*",
            Div => "/",
            Remainder => "%",
            LShift => "<<",
            RShift => ">>",
            And => "&",
            AndNot => "&^",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(u64),
    // TODO: Float, Imag, Rune
    String(Atom),
    Ident(Atom),
    // TODO: CompositeLit
    Function {
        sig: Signature,
        body: Block,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ExprId(u32);
impl_id!(ExprId);

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub id: ExprId,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span, id: ExprId) -> Expr {
        Expr { kind, span, id }
    }
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum IfStmtTail {
    None,
    ElseIf(P<IfStmt>),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub pre_stmt: Option<SimpleStmt>,
    pub cond: P<Expr>,
    pub then: Block,
    pub els: IfStmtTail,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RangeClauseLeft {
    Exprs(List<Expr>),
    Idents(List<Atom>),
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct RangeClause {
    pub left: RangeClauseLeft,
    pub right: P<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub header: ForStmtHeader,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSpec {
    pub kind: TypeSpecKind,
    pub ident: Atom,
    pub ty: P<Type>,
}

/// Invariant: either `ty` or `exprs` must be `Some`
#[derive(Debug, Clone, PartialEq)]
pub struct VarSpec {
    pub idents: List<Atom>,
    pub ty: Option<P<Type>>,
    pub exprs: Option<List<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Const(List<ConstSpec>),
    Type(List<TypeSpec>),
    Var(List<VarSpec>),
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum ImportAlias {
    None,
    Splat,
    Name(Atom),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportSpec {
    pub alias: ImportAlias,
    pub path: Atom,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedParameter {
    pub name: Atom,
    pub ty: P<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedParameter {
    pub ty: P<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Parameters {
    Unnamed {
        params: List<UnnamedParameter>,
        vararg: Option<UnnamedParameter>,
    },
    Named {
        params: List<NamedParameter>,
        vararg: Option<NamedParameter>,
    },
}

impl Parameters {
    pub fn is_empty(&self) -> bool {
        match *self {
            Parameters::Unnamed { ref params, ref vararg } => params.is_empty() && vararg.is_none(),
            Parameters::Named { ref params, ref vararg } => params.is_empty() && vararg.is_none(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    pub params: Parameters,
    pub result: Parameters,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: Atom,
    pub sig: Signature,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDecl {
    // Invariants: must contain a single, non-variadic parameter with type of the form T or *T
    pub receiver: Parameters,
    pub name: Atom,
    pub sig: Signature,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelDecl {
    Declaration(Declaration),
    Function(FunctionDecl),
    Method(MethodDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub decls: List<ImportSpec>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub package_name: Atom,

    // Flattened list of imports
    pub imports: List<ImportSpec>,

    pub decls: List<TopLevelDecl>,
}