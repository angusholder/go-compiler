use ptr::{ P, List };

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
    Modulo,
    LShift,
    RShift,
    And,
    Nand,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AssignOp {
    // Add op
    Add,
    Sub,
    Or,
    Xor,

    // Mul op
    Mul,
    Div,
    Modulo,
    LShift,
    RShift,
    And,
    Nand,
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Imag(f64),
    Rune(char),
    String(String),
    Ident(String),
    // TODO: CompositeLit
    Function {
        sig: Signature,
        body: Block,
    }
}

#[derive(Debug)]
pub enum Expr {
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
        right: Ident,
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
        child: P<Expr>,
        ty: P<Type>,
    },
    Call {
        // For make(T, n) and new(T)
        ty: Option<P<Type>>,
        exprs: List<Expr>,
        ellipsis: bool,
    },
}

pub type Label = String;
pub type Block = List<Stmt>;

#[derive(Clone, Debug)]
pub struct Ident(String);

impl Ident {
    pub fn is_blank(&self) -> bool {
        self.0 == "_"
    }
}

#[derive(Debug)]
pub enum BlockOrIf {
    Block(Block),
    If(P<IfStmt>),
}

#[derive(Debug)]
pub struct IfStmt {
    pre_stmt: Option<P<SimpleStmt>>,
    cond: P<Expr>,
    then: Block,
    els: Option<BlockOrIf>,
}

#[derive(Debug)]
pub enum RangeClauseLeft {
    Exprs(List<Expr>),
    Idents(List<Ident>),
}

#[derive(Debug)]
pub enum ForStmtHeader {
    Condition(P<Expr>),
    ForClause {
        init: Option<P<SimpleStmt>>,
        cond: Option<P<Expr>>,
        post: Option<P<SimpleStmt>>,
    },
    RangeClause {
        left: RangeClauseLeft,
        right: P<Expr>,
    }
}

#[derive(Debug)]
pub enum SimpleStmt {
    Empty,
    Expr(P<Expr>),
    Send {
        channel: P<Expr>,
        expr: P<Expr>,
    },
    Inc(P<Expr>),
    Dec(P<Expr>),
    Assignment {
        left: P<Expr>,
        right: P<Expr>,
        op: Option<AssignOp>,
    },
    ShortVarDecl {
        idents: List<Ident>,
        exprs: List<Expr>,
    },
}

#[derive(Debug)]
pub struct ConstSpec {
    left: List<Ident>,
    right: (Option<P<Type>>, List<Expr>),
}

#[derive(Debug)]
pub enum TypeSpec {
    AliasDecl {
        ident: Ident,
        ty: P<Type>,
    },
    TypeDef {
        ident: Ident,
        ty: P<Type>,
    }
}

#[derive(Debug)]
pub struct VarSpec {
    idents: List<Ident>,
    ty: Option<Type>,
    exprs: Option<List<Expr>>,
}

#[derive(Debug)]
pub enum Declaration {
    Const(List<ConstSpec>),
    Type(TypeSpec),
    Var(VarSpec),
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
    For {
        header: ForStmtHeader,
        body: Block,
    },
    Defer(P<Expr>),
}

#[derive(Debug)]
pub enum Type {
    TypeName {
        ident: Ident,
        package: Option<String>,
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
        length: usize,
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
    Name(Ident),
}

#[derive(Debug)]
pub struct ImportSpec {
    alias: ImportAlias,
    path: String,
}

#[derive(Debug)]
pub struct ParameterDecl {
    idents: List<Ident>,
    ellipsis: bool,
    ty: P<Type>,
}

#[derive(Debug)]
pub enum Result {
    None,
    One(P<Type>),
    Many(ParameterDecl),
}

#[derive(Debug)]
pub struct Signature {
    params: List<ParameterDecl>,
    result: Result,
}

#[derive(Debug)]
pub struct FunctionDecl {
    name: Ident,
    sig: Signature,
    body: Option<Block>,
}

#[derive(Debug)]
pub struct MethodDecl {
    // Invariants: must contain a single, non-variadic parameter with type of the form T or *T
    receiver: ParameterDecl,
    name: Ident,
    sig: Signature,
    body: Option<Block>,
}

#[derive(Debug)]
pub enum TopLevelDecl {
    Declaration(Declaration),
    Function(FunctionDecl),
    Method(MethodDecl),
}

#[derive(Debug)]
pub struct SourceFile {
    package_clause: Ident,

    // Flattened list of imports
    imports: List<ImportSpec>,

    decls: List<TopLevelDecl>,
}