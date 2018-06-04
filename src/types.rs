use std::fmt::Debug;
use std::hash::Hash;
use std::rc::{Rc, Weak};
use syntax::ast;
use syntax::codemap;

pub type P<T> = Rc<T>;
pub type Ident = String;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl From<codemap::Span> for Span {
    fn from(span: codemap::Span) -> Span {
        let codemap::BytePos(lo) = span.lo();
        let codemap::BytePos(hi) = span.hi();
        Span { lo, hi }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> From<codemap::Spanned<T>> for Spanned<T> {
    fn from(spanned: codemap::Spanned<T>) -> Spanned<T> {
        let span = spanned.span.into();
        Spanned {
            node: spanned.node,
            span,
        }
    }
}

pub type BinOp = Spanned<ast::BinOpKind>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Node {
    Ty(P<Ty>),
    Expr(P<Expr>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct AstNode<T: Clone + PartialEq + Eq + Hash + Debug> {
    pub parent: Node,
    pub span: Span,
    pub node: T,
}

pub type Ty = AstNode<TyKind>;
pub type Expr = AstNode<ExprKind>;
pub type Pat = AstNode<PatKind>;
pub type Block = AstNode<()>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub ident: Ident,
    pub parameters: Vec<Ident>,
}

impl From<String> for PathSegment {
    fn from(ident: String) -> Self {
        PathSegment {
            ident,
            parameters: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathParameters {
    Normal(),
    Function(),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum PatKind {
    /// Represents a wildcard pattern (`_`)
    Wild,
    Ident(Ident),
    /// A struct or struct variant pattern, e.g. `Variant {x, y, ..}`.
    /// The `bool` is `true` in the presence of a `..`.
    Struct(Path, Vec<Spanned<FieldPat>>, bool),

    /// A tuple struct/variant pattern `Variant(x, y, .., z)`.
    /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
    /// 0 <= position <= subpats.len()
    TupleStruct(Path, Vec<P<Pat>>, Option<usize>),

    /// A possibly qualified path pattern.
    /// Unqualified path patterns `A::B::C` can legally refer to variants, structs, constants
    /// or associated constants. Qualified path patterns `<A>::B::C`/`<A as Trait>::B::C` can
    /// only legally refer to associated constants.
    Path(Option<QSelf>, Path),

    /// A tuple pattern `(a, b)`.
    /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
    /// 0 <= position <= subpats.len()
    Tuple(Vec<P<Pat>>, Option<usize>),
    /// A `box` pattern
    Box(P<Pat>),
    /// A reference pattern, e.g. `&mut (a, b)`
    Ref(P<Pat>, Mutability),
    /// A literal
    Lit(P<Expr>),
    /// A range pattern, e.g. `1...2`, `1..=2` or `1..2`
    Range(P<Expr>, P<Expr>, RangeEnd),
    /// `[a, b, ..i, y, z]` is represented as:
    ///     `PatKind::Slice(box [a, b], Some(i), box [y, z])`
    Slice(Vec<P<Pat>>, Option<P<Pat>>, Vec<P<Pat>>),
    /// Parentheses in patters used for grouping, i.e. `(PAT)`.
    Paren(P<Pat>),
    /// A macro pattern; pre-expansion
    Mac(Mac),
}

pub struct LocalEntity {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum StmtKind {
    Local,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ExprKind {
    /// A `box x` expression.
    Box(P<Expr>),
    /// First expr is the place; second expr is the value.
    ObsoleteInPlace(P<Expr>, P<Expr>),
    /// An array (`[a, b, c, d]`)
    Array(Vec<P<Expr>>),
    /// A function call
    ///
    /// The first field resolves to the function itself,
    /// and the second field is the list of arguments.
    /// This also represents calling the constructor of
    /// tuple-like ADTs such as tuple structs and enum variants.
    Call(P<Expr>, Vec<P<Expr>>),
    /// A method call (`x.foo::<'static, Bar, Baz>(a, b, c, d)`)
    ///
    /// The `PathSegment` represents the method name and its generic arguments
    /// (within the angle brackets).
    /// The first element of the vector of `Expr`s is the expression that evaluates
    /// to the object on which the method is being called on (the receiver),
    /// and the remaining elements are the rest of the arguments.
    /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
    /// `ExprKind::MethodCall(PathSegment { foo, [Bar, Baz] }, [x, a, b, c, d])`.
    MethodCall(PathSegment, Vec<P<Expr>>),
    /// A tuple (`(a, b, c ,d)`)
    Tup(Vec<P<Expr>>),
    /// A binary operation (For example: `a + b`, `a * b`)
    Binary(BinOp, P<Expr>, P<Expr>),
    /// A unary operation (For example: `!x`, `*x`)
    Unary(ast::UnOp, P<Expr>),
    /// A literal (For example: `1`, `"foo"`)
    Lit,
    /// A cast (`foo as f64`)
    Cast(P<Expr>, P<Ty>),
    Type(P<Expr>, P<Ty>),
    /// An `if` block, with an optional else block
    ///
    /// `if expr { block } else { expr }`
    If(P<Expr>, P<Block>, Option<P<Expr>>),
    /// An `if let` expression with an optional else block
    ///
    /// `if let pat = expr { block } else { expr }`
    ///
    /// This is desugared to a `match` expression.
    IfLet(Vec<P<Pat>>, P<Expr>, P<Block>, Option<P<Expr>>),
    /// A while loop, with an optional label
    ///
    /// `'label: while expr { block }`
    While(P<Expr>, P<Block>, Option<Label>),
    /// A while-let loop, with an optional label
    ///
    /// `'label: while let pat = expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    WhileLet(Vec<P<Pat>>, P<Expr>, P<Block>, Option<Label>),
    /// A for loop, with an optional label
    ///
    /// `'label: for pat in expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    ForLoop(P<Pat>, P<Expr>, P<Block>, Option<Label>),
    /// Conditionless loop (can be exited with break, continue, or return)
    ///
    /// `'label: loop { block }`
    Loop(P<Block>, Option<Label>),
    /// A `match` block.
    Match(P<Expr>, Vec<Arm>),
    /// A closure (for example, `move |a, b, c| a + b + c`)
    ///
    /// The final span is the span of the argument block `|...|`
    Closure(CaptureBy, Movability, P<FnDecl>, P<Expr>, Span),
    /// A block (`'label: { ... }`)
    Block(P<Block>, Option<Label>),
    /// A catch block (`catch { ... }`)
    Catch(P<Block>),

    /// An assignment (`a = foo()`)
    Assign(P<Expr>, P<Expr>),
    /// An assignment with an operator
    ///
    /// For example, `a += 1`.
    AssignOp(BinOp, P<Expr>, P<Expr>),
    /// Access of a named (`obj.foo`) or unnamed (`obj.0`) struct field
    Field(P<Expr>, Ident),
    /// An indexing operation (`foo[2]`)
    Index(P<Expr>, P<Expr>),
    /// A range (`1..2`, `1..`, `..2`, `1...2`, `1...`, `...2`)
    Range(Option<P<Expr>>, Option<P<Expr>>, RangeLimits),

    /// Variable reference, possibly containing `::` and/or type
    /// parameters, e.g. foo::bar::<baz>.
    ///
    /// Optionally "qualified",
    /// E.g. `<Vec<T> as SomeTrait>::SomeType`.
    Path(Option<QSelf>, Path),

    /// A referencing operation (`&a` or `&mut a`)
    AddrOf(Mutability, P<Expr>),
    /// A `break`, with an optional label to break, and an optional expression
    Break(Option<Label>, Option<P<Expr>>),
    /// A `continue`, with an optional label
    Continue(Option<Label>),
    /// A `return`, with an optional value to be returned
    Ret(Option<P<Expr>>),

    /// Output of the `asm!()` macro
    InlineAsm(P<InlineAsm>),

    /// A macro invocation; pre-expansion
    Mac(Mac),

    /// A struct literal expression.
    ///
    /// For example, `Foo {x: 1, y: 2}`, or
    /// `Foo {x: 1, .. base}`, where `base` is the `Option<Expr>`.
    Struct(Path, Vec<Field>, Option<P<Expr>>),

    /// An array literal constructed from one repeated element.
    ///
    /// For example, `[1; 5]`. The expression is the element to be
    /// repeated; the constant is the number of times to repeat it.
    Repeat(P<Expr>, AnonConst),

    /// No-op: used solely so we can pretty-print faithfully
    Paren(P<Expr>),

    /// `expr?`
    Try(P<Expr>),

    /// A `yield`, with an optional value to be yielded
    Yield(Option<P<Expr>>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TyKind {
    /// A variable-length slice (`[T]`)
    Slice(P<Ty>),
    /// A fixed length array (`[T; n]`)
    Array(P<Ty>, AnonConst),
    /// A raw pointer (`*const T` or `*mut T`)
    Ptr(MutTy),
    /// A reference (`&'a T` or `&'a mut T`)
    Rptr(Option<Lifetime>, MutTy),
    /// A bare function (e.g. `fn(usize) -> bool`)
    BareFn(P<BareFnTy>),
    /// The never type (`!`)
    Never,
    /// A tuple (`(A, B, C, D,...)`)
    Tup(Vec<P<Ty>>),
    /// A path (`module::module::...::Type`), optionally
    /// "qualified", e.g. `<Vec<T> as SomeTrait>::SomeType`.
    ///
    /// Type parameters are stored in the Path itself
    Path(Option<QSelf>, Path),
    /// A trait object type `Bound1 + Bound2 + Bound3`
    /// where `Bound` is a trait or a lifetime.
    TraitObject(TyParamBounds, TraitObjectSyntax),
    /// An `impl Bound1 + Bound2 + Bound3` type
    /// where `Bound` is a trait or a lifetime.
    ImplTrait(TyParamBounds),
    /// No-op; kept solely so that we can pretty-print faithfully
    Paren(P<Ty>),
    /// Unused for now
    Typeof(AnonConst),
    /// TyKind::Infer means the type should be inferred instead of it having been
    /// specified. This can appear anywhere in a type.
    Infer,
    /// Inferred type of a `self` or `&self` argument in a method.
    ImplicitSelf,
    // A macro in the type position.
    Mac(Mac),
    /// Placeholder for a kind that has failed to be defined.
    Err,
}
