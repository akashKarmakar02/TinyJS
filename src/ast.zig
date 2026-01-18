const lex = @import("lexer.zig");
pub const Span = lex.Span;

pub const Program = struct {
    statements: []const Stmt,
};

pub const Stmt = union(enum) {
    import_stmt: ImportStmt,
    var_decl: VarDecl,
    function_decl: FunctionDecl,
    return_stmt: ReturnStmt,
    if_stmt: IfStmt,
    for_stmt: ForStmt,
    for_of_stmt: ForOfStmt,
    while_stmt: WhileStmt,
    break_stmt,
    continue_stmt,
    block: Block,
    assign: Assign,
    expr_stmt: ExprStmt,
};

pub const ImportStmt = struct {
    bind_name: ?[]const u8, // `import name from "spec"` or null for `import "spec"`
    spec: []const u8,
};

pub const VarKind = enum { @"let", @"const", @"var" };

pub const VarDecl = struct {
    kind: VarKind,
    name: []const u8,
    name_span: Span,
    init: ?*Expr,
};

pub const Assign = struct {
    name: []const u8,
    name_span: Span,
    op: AssignOp = .assign,
    value: *Expr,
};

pub const AssignOp = enum {
    assign,
    add_assign,
    sub_assign,
    mul_assign,
    div_assign,
    mod_assign,
};

pub const ExprStmt = struct {
    expr: *Expr,
};

pub const Block = struct {
    span: Span,
    statements: []const Stmt,
};

pub const FunctionDecl = struct {
    name: []const u8,
    params: []const []const u8,
    body: Block,
    span: Span,
};

pub const ReturnStmt = struct {
    value: ?*Expr,
};

pub const IfStmt = struct {
    condition: *Expr,
    then_branch: *Stmt,
    else_branch: ?*Stmt,
};

pub const WhileStmt = struct {
    condition: *Expr,
    body: *Stmt,
};

pub const ForStmt = struct {
    init: ?ForClause,
    condition: ?*Expr,
    post: ?ForClause,
    body: *Stmt,
};

pub const ForOfStmt = struct {
    kind: VarKind,
    name: []const u8,
    name_span: Span,
    iterable: *Expr,
    body: *Stmt,
};

pub const ForClause = union(enum) {
    var_decl: VarDecl,
    assign: Assign,
    expr: *Expr,
};

pub const Expr = union(enum) {
    number: f64,
    string: []const u8,
    ident: Ident,
    this,
    boolean: bool,
    null,
    undefined,
    object_literal: ObjectLiteral,
    array_literal: ArrayLiteral,
    update: Update,
    unary: Unary,
    binary: Binary,
    logical: Logical,
    index: Index,
    assign_expr: AssignExpr,
    function_expr: FunctionExpr,
    member: Member,
    call: Call,
};

pub const Ident = struct {
    name: []const u8,
    span: Span,
};

pub const ObjectLiteral = struct {
    props: []const ObjectProp,
};

pub const ObjectProp = struct {
    key: []const u8,
    value: *Expr,
};

pub const ArrayLiteral = struct {
    items: []const *Expr,
};

pub const UnaryOp = enum { neg, not };

pub const Unary = struct {
    op: UnaryOp,
    expr: *Expr,
};

pub const UpdateOp = enum { inc, dec };

pub const Update = struct {
    op: UpdateOp,
    target: Ident, // identifier only for now
    prefix: bool,
};

pub const BinaryOp = enum {
    add,
    sub,
    mul,
    div,
    mod,
    eq,
    neq,
    strict_eq,
    strict_neq,
    lt,
    lte,
    gt,
    gte,
};

pub const Binary = struct {
    op: BinaryOp,
    left: *Expr,
    right: *Expr,
};

pub const LogicalOp = enum { @"and", @"or", nullish };

pub const Logical = struct {
    op: LogicalOp,
    left: *Expr,
    right: *Expr,
};

pub const Index = struct {
    object: *Expr,
    index: *Expr,
};

pub const AssignExpr = struct {
    op: AssignOp,
    target: *Expr, // ident/member/index only
    value: *Expr,
};

pub const FunctionExpr = struct {
    name: ?[]const u8, // optional (named function expression)
    params: []const []const u8,
    is_arrow: bool = false,
    body: Block,
    span: Span,
};

pub const Member = struct {
    object: *Expr,
    property: []const u8,
};

pub const Call = struct {
    callee: *Expr,
    args: []const *Expr,
};
