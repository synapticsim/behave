use std::ops::{Add, AddAssign};

use crate::{diagnostic::Location, syntax::token::Span};

#[derive(Clone, Copy, Debug)]
pub struct Loc<'a> {
	pub span: Span,
	pub file: &'a str,
}

impl<'a> Add for Loc<'a> {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		assert_eq!(self.file, rhs.file);
		self + rhs.span
	}
}

impl<'a> Add<Span> for Loc<'a> {
	type Output = Self;

	fn add(self, rhs: Span) -> Self::Output {
		Self {
			span: Span {
				start: self.span.start.min(rhs.start),
				end: self.span.end.max(rhs.end),
			},
			file: self.file,
		}
	}
}

impl<'a> AddAssign for Loc<'a> {
	fn add_assign(&mut self, rhs: Self) { *self = self.clone() + rhs; }
}

impl<'a> AddAssign<Span> for Loc<'a> {
	fn add_assign(&mut self, rhs: Span) { *self = self.clone() + rhs; }
}

impl Into<Location> for Loc<'_> {
	fn into(self) -> Location {
		Location {
			span: self.span.into(),
			file: self.file.to_string(),
		}
	}
}

type P<T> = Box<T>;

#[derive(Debug)]
pub struct Spanned<'a, T> {
	pub node: T,
	pub loc: Loc<'a>,
}

#[derive(Debug)]
pub struct AST<'a> {
	pub lods: (Vec<(Expr<'a>, Expr<'a>)>, Loc<'a>),
	pub behavior: Option<(Block<'a>, Loc<'a>)>,
	pub items: Vec<Item<'a>>,
}

pub type Stmt<'a> = Spanned<'a, StmtKind<'a>>;

pub type Expr<'a> = Spanned<'a, ExprKind<'a>>;

pub type BinOp<'a> = Spanned<'a, BinOpKind>;

pub type UnOp<'a> = Spanned<'a, UnOpKind>;

pub type Lit<'a> = Spanned<'a, LitKind<'a>>;

pub type Pat<'a> = Spanned<'a, PatKind<'a>>;

pub type Block<'a> = Spanned<'a, Vec<Stmt<'a>>>;

pub type Ty<'a> = Spanned<'a, TyKind<'a>>;

pub type Ident<'a> = Spanned<'a, &'a str>;

pub type Path<'a> = Spanned<'a, Vec<Ident<'a>>>;

pub type Item<'a> = Spanned<'a, ItemKind<'a>>;

#[derive(Debug)]
pub enum ExprKind<'a> {
	None,
	Lit(Lit<'a>),
	Array(Vec<Expr<'a>>),
	Map(Vec<(Expr<'a>, Expr<'a>)>),
	EmptyMapOrArray,
	Tuple(Vec<Expr<'a>>),
	Call(P<Expr<'a>>, Vec<Expr<'a>>),
	Binary(P<(Expr<'a>, BinOp<'a>, Expr<'a>)>),
	Unary(UnOp<'a>, P<Expr<'a>>),
	Cast(P<Expr<'a>>, Ty<'a>),
	Is(P<Expr<'a>>, Ty<'a>),
	If(P<Expr<'a>>, Block<'a>, Option<P<Expr<'a>>>),
	While(P<Expr<'a>>, Block<'a>),
	For(Pat<'a>, P<Expr<'a>>, Block<'a>),
	Switch(P<Expr<'a>>, Vec<Arm<'a>>),
	Func(FnDecl<'a>, P<Expr<'a>>),
	RPNFunc(FnDecl<'a>, P<Expr<'a>>),
	Block(Block<'a>),
	Assign(P<(Expr<'a>, Loc<'a>, Expr<'a>)>),
	AssignOp(P<Expr<'a>>, BinOp<'a>, P<Expr<'a>>), //
	Field(P<Expr<'a>>, Ident<'a>),
	Index(P<(Expr<'a>, Expr<'a>)>),
	Path(Path<'a>),
	Break,
	Continue,
	Ret(Option<P<Expr<'a>>>),
	Struct(StructExpr<'a>),
	Paren(P<Expr<'a>>),
	BehExpr(P<BehExprKind<'a>>), //
	RPNAccess(P<RPNAccess<'a>>),
	Ref(P<Expr<'a>>),
	Err,
}

#[derive(Debug)]
pub enum StmtKind<'a> {
	Item(Item<'a>),
	Expr(Expr<'a>),
	Semi(Expr<'a>),
	Local(Local<'a>),
}

#[derive(Debug)]
pub enum BinOpKind {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	And,
	Or,
	Eq,
	Ne,
	Lt,
	Gt,
	Le,
	Ge,
}

#[derive(Debug)]
pub enum UnOpKind {
	Not,
	Neg,
}

#[derive(Debug)]
pub enum LitKind<'a> {
	Str(&'a str),
	Int(i64),
	Float(f64),
	Bool(bool),
	Err(&'a str),
}

#[derive(Debug)]
pub enum PatKind<'a> {
	Ignore,
	Binding(Ident<'a>),
	Destructure(Vec<Pat<'a>>),
}

#[derive(Debug)]
pub enum RPNType {
	Sim,
	Input,
	Env,
	Gauge,
	HTML,
	Instrument,
	Key { two_arg: bool },
	Local,
	Mouse,
	Component,
	Resource { legacy: bool },
	Wwise,
	Custom,
}

#[derive(Debug)]
pub struct RPNAccess<'a> {
	pub rpn_ty: RPNType,
	pub var: Expr<'a>,
	pub unit: Option<Ty<'a>>,
}

#[derive(Debug)]
pub struct Arm<'a> {
	pub value: Expr<'a>,
	pub body: Expr<'a>,
	pub loc: Loc<'a>,
}

#[derive(Debug)]
pub struct Param<'a> {
	pub ident: Ident<'a>,
	pub ty: Option<Ty<'a>>,
	pub default: Option<Expr<'a>>,
	pub loc: Loc<'a>,
}

#[derive(Debug)]
pub enum RetTy<'a> {
	Default(Loc<'a>),
	Ty(Ty<'a>),
}

#[derive(Debug)]
pub struct FnDecl<'a> {
	pub inputs: Vec<Param<'a>>,
	pub output: RetTy<'a>,
}

#[derive(Debug)]
pub struct Local<'a> {
	pub pat: Pat<'a>,
	pub ty: Option<Ty<'a>>,
	pub init: Option<Expr<'a>>,
}

#[derive(Debug)]
pub struct ExprField<'a> {
	pub ident: Ident<'a>,
	pub expr: Expr<'a>,
}

#[derive(Debug)]
pub struct StructExpr<'a> {
	pub path: Path<'a>,
	pub fields: Vec<ExprField<'a>>,
}

#[derive(Debug)]
pub enum TyKind<'a> {
	Array(P<Ty<'a>>),
	Map(P<(Ty<'a>, Ty<'a>)>),
	Func(Vec<Ty<'a>>, P<RetTy<'a>>),
	RPNFunc(Vec<Ty<'a>>, P<RetTy<'a>>),
	Sum(Vec<Ty<'a>>),
	Tuple(Vec<Ty<'a>>),
	Optional(P<Ty<'a>>),
	Path(Path<'a>),
	Ref(P<Ty<'a>>),
	Infer,
}

#[derive(Debug)]
pub enum ImportTreeKind<'a> {
	Simple(Option<Ident<'a>>),
	Nested(Vec<ImportTree<'a>>),
	Glob,
}

#[derive(Debug)]
pub struct ImportTree<'a> {
	pub prefix: Path<'a>,
	pub kind: ImportTreeKind<'a>,
}

#[derive(Debug)]
pub struct Mod<'a> {
	pub items: Vec<Item<'a>>,
	pub inline: bool,
}

#[derive(Debug)]
pub struct Variant<'a> {
	pub ident: Ident<'a>,
	pub disc: Option<Lit<'a>>,
	pub loc: Loc<'a>,
}

#[derive(Debug)]
pub struct ImplKind<'a> {
	pub on: Ty<'a>,
	pub items: Vec<(Ident<'a>, FnDecl<'a>, Block<'a>, Loc<'a>)>,
}

#[derive(Debug)]
pub enum ItemKind<'a> {
	Import(ImportTree<'a>),
	Fn(Ident<'a>, FnDecl<'a>, Block<'a>),
	RPNFn(Ident<'a>, FnDecl<'a>, Block<'a>),
	Template(Ident<'a>, Vec<Param<'a>>, Block<'a>),
	Mod(Ident<'a>, Mod<'a>),
	Enum(Ident<'a>, Vec<Variant<'a>>),
	Struct(Ident<'a>, Vec<Param<'a>>),
	Impl(ImplKind<'a>),
}

#[derive(Debug)]
pub enum BehExprKind<'a> {
	Use(StructExpr<'a>),
	Component(Expr<'a>, Option<Expr<'a>>, Block<'a>),
	Animation(Expr<'a>, Vec<ExprField<'a>>),
	Visibility(Expr<'a>),
	Emissive(Expr<'a>),
	Interaction(Vec<ExprField<'a>>),
	Events(Expr<'a>, Expr<'a>),
	Update(Vec<ExprField<'a>>),
	InputEvent(Expr<'a>, Vec<ExprField<'a>>),
}
