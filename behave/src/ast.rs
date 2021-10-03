use std::collections::HashMap;
use std::ops::Range;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::diagnostic::{Diagnostic, Level};
use crate::items::{EnumId, FunctionId, ItemMap, StructId, TemplateId};

#[derive(Clone, Debug)]
pub enum ASTTree<'a> {
	Branch(HashMap<String, ASTTree<'a>>),
	Leaf(AST<'a>),
}

impl<'a> ASTTree<'a> {
	pub fn new() -> Self { Self::Branch(HashMap::new()) }

	pub fn add_ast(&mut self, path: &[String], ast: AST<'a>) -> bool {
		match self {
			Self::Branch(ref mut map) => {
				if path.len() == 1 {
					map.insert(path[0].clone(), ASTTree::Leaf(ast));
					true
				} else {
					let tree = map.entry(path[0].clone()).or_insert(ASTTree::new());
					tree.add_ast(&path[1..], ast)
				}
			},
			_ => false,
		}
	}

	pub fn get_ast(&self, path: &[Ident]) -> Result<&ASTTree, Diagnostic> {
		match self {
			Self::Branch(map) => {
				if path.len() == 0 {
					Ok(self)
				} else if let Some(ast) = map.get(&path[0].0) {
					ast.get_ast(&path[1..])
				} else {
					Err(Diagnostic::new(Level::Error, "path does not exist"))
				}
			},
			Self::Leaf(_) => {
				if path.len() == 0 {
					Ok(self)
				} else {
					Err(Diagnostic::new(Level::Error, "path refers to file as folder"))
				}
			},
		}
	}
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Location<'a> {
	pub range: Range<usize>,
	pub file: &'a [String],
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASTType<'a> {
	Main(LODs<'a>, Behavior<'a>),
	Secondary(Vec<Item<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AST<'a> {
	pub imports: Vec<Import<'a>>,
	pub ast_data: ASTType<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LOD<'a> {
	pub min_size: Expression<'a>,
	pub file: Expression<'a>,
	pub loc: Location<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LODs<'a>(pub Vec<LOD<'a>>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct Behavior<'a>(pub Vec<Statement<'a>>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub enum ItemType<'a> {
	Function(Ident<'a>, FunctionId),
	Template(TemplateId),
	Struct(StructId),
	Enum(EnumId),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable<'a> {
	pub name: Ident<'a>,
	pub value: Option<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant<'a> {
	pub name: Ident<'a>,
	pub value: usize,
	pub loc: Location<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enum<'a> {
	pub name: Ident<'a>,
	pub variants: Vec<EnumVariant<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct<'a> {
	pub name: Ident<'a>,
	pub fields: Vec<VarEntry<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item<'a>(pub ItemType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType<'a> {
	pub args: Vec<Type<'a>>,
	pub ret: Option<Box<Type<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Template<'a> {
	pub name: Ident<'a>,
	pub args: Vec<VarEntry<'a>>,
	pub block: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeType<'a> {
	Num,
	Str,
	Bool,
	Other(OtherType<'a>),
	Array(Box<Type<'a>>),
	Map(Box<Type<'a>>, Box<Type<'a>>),
	Sum(Vec<Type<'a>>),
	Function(FunctionType<'a>),
	Code,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OtherType<'a> {
	pub path: Path<'a>,
	pub resolved: Option<ResolvedType>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ResolvedType {
	Struct(StructType),
	Enum(EnumType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type<'a>(pub TypeType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct VarEntry<'a> {
	pub name: Ident<'a>,
	pub ty: Type<'a>,
	pub default: Option<Box<Expression<'a>>>,
	pub loc: Location<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionType<'a> {
	None,
	String(String),
	Number(f64),
	Boolean(bool),
	Block(Block<'a>),
	Function(Function<'a>),
	Code(Block<'a>),
	Array(Vec<Expression<'a>>),
	Map(Vec<(Expression<'a>, Expression<'a>)>),
	Access(Access<'a>),
	StructCreate(StructCreate<'a>),
	RPNAccess(Box<Expression<'a>>),
	Index(Index<'a>),
	Assignment(Assignment<'a>),
	Unary(UnaryOperator, Box<Expression<'a>>),
	Binary(Box<Expression<'a>>, BinaryOperator, Box<Expression<'a>>),
	Call(Call<'a>),
	IfChain(IfChain<'a>),
	Switch(Switch<'a>),
	While(While<'a>),
	For(For<'a>),
	Is(Box<Expression<'a>>, Type<'a>),
	As(Box<Expression<'a>>, Type<'a>),
	Return(Option<Box<Expression<'a>>>),
	Break(Option<Box<Expression<'a>>>),
	Behavior(BehaviorExpression<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression<'a>(pub ExpressionType<'a>, pub Location<'a>);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
	Negate,
	Not,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
	Add,
	Subtract,
	Multiply,
	Divide,
	And,
	Or,
	Equal,
	NotEqual,
	Greater,
	Lesser,
	GreaterThanOrEqual,
	LesserThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BehaviorExpression<'a> {
	Use(Use<'a>),
	Component(Component<'a>),
	Animation(Animation<'a>),
	Visible(Box<Expression<'a>>),
	Emissive(Box<Expression<'a>>),
	Interaction(Interaction<'a>),
	Events(Box<Expression<'a>>, Box<Expression<'a>>),
	Update(Update<'a>),
	InputEvent(InputEvent<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructCreate<'a> {
	pub ty: Type<'a>,
	pub values: Vec<(Ident<'a>, Expression<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Index<'a> {
	pub array: Box<Expression<'a>>,
	pub index: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Access<'a> {
	pub path: Path<'a>,
	pub resolved: Option<ResolvedAccess>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedAccess {
	Local,
	Global(GlobalAccess),
}

#[derive(Clone, Debug, PartialEq)]
pub enum GlobalAccess {
	Enum(EnumAccess),
	Function(FunctionAccess),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionAccess {
	User(FunctionId),
	Inbuilt(InbuiltFunction),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InbuiltFunction {
	Format,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EnumType {
	User(EnumId),
	Inbuilt(InbuiltEnum),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StructType {
	User(StructId),
	Inbuilt(InbuiltStruct),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InbuiltEnum {
	MouseEvent,
	Cursor,
	Hitbox,
	Direction,
	Icon,
	InteractionTip,
	InteractionMode,
	Axis,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InbuiltStruct {
	Cursors,
	Event,
	AnimTooltip,
	NormalDrag,
	TrajectoryDrag,
	Setter,
	Binding,
}

impl InbuiltEnum {
	pub fn to_string(self) -> String {
		match self {
			InbuiltEnum::MouseEvent => "MouseEvent",
			InbuiltEnum::Cursor => "Cursor",
			InbuiltEnum::Hitbox => "Hitbox",
			InbuiltEnum::Direction => "Direction",
			InbuiltEnum::Icon => "Icon",
			InbuiltEnum::InteractionTip => "InteractionTip",
			InbuiltEnum::InteractionMode => "Interaction",
			InbuiltEnum::Axis => "Axis",
		}
		.to_string()
	}
}

impl InbuiltStruct {
	pub fn to_string(self) -> String {
		match self {
			InbuiltStruct::Cursors => "Cursors",
			InbuiltStruct::Event => "Event",
			InbuiltStruct::AnimTooltip => "AnimTooltip",
			InbuiltStruct::NormalDrag => "NormalDrag",
			InbuiltStruct::TrajectoryDrag => "TrajectoryDrag",
			InbuiltStruct::Setter => "Setter",
			InbuiltStruct::Binding => "Binding",
		}
		.to_string()
	}
}

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq)]
pub enum MouseEvent {
	RightSingle,
	MiddleSingle,
	LeftSingle,
	RightDouble,
	MiddleDouble,
	LeftDouble,
	RightDrag,
	MiddleDrag,
	LeftDrag,
	RightRelease,
	MiddleRelease,
	LeftRelease,
	Lock,
	Unlock,
	Move,
	Leave,
	WheelUp,
	WheelDown,
}

impl MouseEvent {
	pub fn to_string(self) -> &'static str {
		use MouseEvent::*;
		match self {
			RightSingle => "RightSingle",
			MiddleSingle => "MiddleSingle",
			LeftSingle => "LeftSingle",
			RightDouble => "RightDouble",
			MiddleDouble => "MiddleDouble",
			LeftDouble => "LeftDouble",
			RightDrag => "RightDrag",
			MiddleDrag => "MiddleDrag",
			LeftDrag => "LeftDrag",
			RightRelease => "RightRelease",
			MiddleRelease => "MiddleRelease",
			LeftRelease => "LeftRelease",
			Lock => "Lock",
			Unlock => "Unlock",
			Move => "Move",
			Leave => "Leave",
			WheelUp => "WheelUp",
			WheelDown => "WheelDown",
		}
	}

	pub fn from_num(val: usize) -> MouseEvent { FromPrimitive::from_usize(val).unwrap() }
}

#[derive(Clone, Copy, Debug, Hash, FromPrimitive, PartialEq, Eq)]
pub enum Cursor {
	None,
	UpArrow,
	DownArrow,
	LeftArrow,
	RightArrow,
	Hand,
	Crosshair,
	Grab,
	GrabOpen,
	TurnLeft,
	TurnRight,
	TurnLeftSmall,
	TurnRightSmall,
	TurnLeftUpsideDown,
	TurnRightUpsideDown,
	UpDownArrows,
	LeftRightArrows,
	DiagonalArrows,
}

impl Cursor {
	pub fn to_string(self) -> &'static str {
		use Cursor::*;
		match self {
			None => "None",
			UpArrow => "UpArrow",
			DownArrow => "DownArrow",
			LeftArrow => "LeftArrow",
			RightArrow => "RightArrow",
			Hand => "Hand",
			Crosshair => "Crosshair",
			Grab => "Grab",
			GrabOpen => "GrabOpen",
			TurnLeft => "TurnLeft",
			TurnRight => "TurnRight",
			TurnLeftSmall => "TurnLeftSmall",
			TurnRightSmall => "TurnRightSmall",
			TurnLeftUpsideDown => "TurnLeftUpside",
			TurnRightUpsideDown => "TurnRightUpsid",
			UpDownArrows => "UpDownArrows",
			LeftRightArrows => "LeftRightArrow",
			DiagonalArrows => "DiagonalArrows",
		}
	}
}

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq, Hash)]
pub enum Hitbox {
	Left,
	Right,
	Up,
	Down,
	Center,
}

impl Hitbox {
	pub fn to_string(self) -> &'static str {
		use Hitbox::*;
		match self {
			Left => "Left",
			Right => "Right",
			Up => "Up",
			Down => "Down",
			Center => "Center",
		}
	}
}

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq, Hash)]
pub enum Direction {
	Forward,
	Backward,
	Both,
}

impl Direction {
	pub fn to_string(self) -> &'static str {
		use Direction::*;
		match self {
			Forward => "Forward",
			Backward => "Backward",
			Both => "Both",
		}
	}
}

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq, Hash)]
pub enum Icon {
	MoveX,
	MoveY,
	MoveAxis,
	MoveAxisX,
	MoveAxisY,
	Rotate,
	Push,
	Pull,
}

impl Icon {
	pub fn to_string(self) -> &'static str {
		use Icon::*;
		match self {
			MoveX => "MoveX",
			MoveY => "MoveY",
			MoveAxis => "MoveAxis",
			MoveAxisX => "MoveAxisX",
			MoveAxisY => "MoveAxisY",
			Rotate => "Rotate",
			Push => "Push",
			Pull => "Pull",
		}
	}
}

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq, Hash)]
pub enum InteractionTip {
	XAxis,
	XAxisLeft,
	XAxisRight,
	YAxis,
	YAxisDown,
	YAxisUp,
	PrimaryUp,
	PrimaryDown,
	SecondaryUp,
	SecondaryDown,
	TertiaryUp,
	TertiaryDown,
	Lock,
	Unlock,
	Increase,
	Decrease,
}

impl InteractionTip {
	pub fn to_string(self) -> &'static str {
		use InteractionTip::*;
		match self {
			XAxis => "XAxis",
			XAxisLeft => "XAxisLeft",
			XAxisRight => "XAxisRight",
			YAxis => "YAxis",
			YAxisDown => "YAxisDown",
			YAxisUp => "YAxisUp",
			PrimaryUp => "PrimaryUp",
			PrimaryDown => "PrimaryDown",
			SecondaryUp => "SecondaryUp",
			SecondaryDown => "SecondaryDown",
			TertiaryUp => "TertiaryUp",
			TertiaryDown => "TertiaryDown",
			Lock => "Lock",
			Unlock => "Unlock",
			Increase => "Increase",
			Decrease => "Decrease",
		}
	}
}

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq, Hash)]
pub enum InteractionMode {
	Legacy,
	Lock,
	Both,
}

impl InteractionMode {
	pub fn to_string(self) -> &'static str {
		use InteractionMode::*;
		match self {
			Legacy => "Legacy",
			Lock => "Lock",
			Both => "Both",
		}
	}
}

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq, Hash)]
pub enum Axis {
	X,
	Y,
	Any,
}

impl Axis {
	pub fn to_string(self) -> &'static str {
		use Axis::*;
		match self {
			X => "X",
			Y => "Y",
			Any => "Any",
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EnumAccess {
	pub id: EnumType,
	pub value: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentTarget<'a> {
	Var(Access<'a>),
	RPNVar(Box<Expression<'a>>),
	Index(Access<'a>, Box<Expression<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
	pub target: AssignmentTarget<'a>,
	pub value: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Switch<'a> {
	pub on: Box<Expression<'a>>,
	pub cases: Vec<Case<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Case<'a> {
	pub value: Box<Expression<'a>>,
	pub code: Box<Expression<'a>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Block<'a> {
	pub statements: Vec<Statement<'a>>,
	pub expression: Option<Box<Expression<'a>>>,
	pub loc: Location<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call<'a> {
	pub callee: Box<Expression<'a>>,
	pub args: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfChain<'a> {
	pub ifs: Vec<(Box<Expression<'a>>, Block<'a>, Location<'a>)>,
	pub else_part: Option<(Block<'a>, Location<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While<'a> {
	pub condition: Box<Expression<'a>>,
	pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct For<'a> {
	pub var: Ident<'a>,
	pub container: Box<Expression<'a>>,
	pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
	pub args: Vec<VarEntry<'a>>,
	pub ret: Option<Type<'a>>,
	pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Use<'a> {
	pub template: UseTarget<'a>,
	pub args: Vec<(Ident<'a>, Expression<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UseTarget<'a> {
	pub path: Path<'a>,
	pub resolved: Option<TemplateId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Component<'a> {
	pub name: Box<Expression<'a>>,
	pub node: Option<Box<Expression<'a>>>,
	pub block: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Animation<'a> {
	pub name: Box<Expression<'a>>,
	pub length: Box<Expression<'a>>,
	pub lag: Box<Expression<'a>>,
	pub value: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Interaction<'a> {
	pub legacy_cursors: Box<Expression<'a>>,
	pub lock_cursors: Box<Expression<'a>>,
	pub legacy_events: Box<Expression<'a>>,
	pub lock_events: Box<Expression<'a>>,
	pub legacy_callback: Box<Expression<'a>>,
	pub lock_callback: Box<Expression<'a>>,
	pub lock_tooltip_title: Box<Expression<'a>>,
	pub lock_tooltips: Box<Expression<'a>>,
	pub animated_tooltips: Box<Expression<'a>>,
	pub cursor_animated: Box<Expression<'a>>,
	pub can_lock: Box<Expression<'a>>,
	pub node_to_highlight: Option<Box<Expression<'a>>>,
	pub drag_mode: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Update<'a> {
	pub frequency: Option<Box<Expression<'a>>>,
	pub mode: Box<Expression<'a>>,
	pub code: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputEvent<'a> {
	pub name: Box<Expression<'a>>,
	pub legacy_icon: Box<Expression<'a>>,
	pub lock_icon: Box<Expression<'a>>,
	pub description: Box<Expression<'a>>,
	pub tooltip_value: Box<Expression<'a>>,
	pub legacy_interaction: Box<Expression<'a>>,
	pub lock_interaction: Box<Expression<'a>>,
	pub value: Box<Expression<'a>>,
	pub units: Box<Expression<'a>>,
	pub init: Box<Expression<'a>>,
	pub watch: Box<Expression<'a>>,
	pub inc: Box<Expression<'a>>,
	pub dec: Box<Expression<'a>>,
	pub set: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementType<'a> {
	Expression(ExpressionType<'a>),
	Declaration(Variable<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement<'a>(pub StatementType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub enum ImportType<'a> {
	Normal(Path<'a>),
	Extern(Expression<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import<'a>(pub ImportType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct Path<'a>(pub Vec<Ident<'a>>, pub Location<'a>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ident<'a>(pub String, pub Location<'a>);

pub trait ASTPass {
	fn lods(&mut self, lods: &mut LODs) {
		for lod in lods.0.iter_mut() {
			self.expression(&mut lod.min_size.0);
			self.expression(&mut lod.file.0);
		}
	}

	fn behavior(&mut self, behavior: &mut Behavior) {
		for stmt in behavior.0.iter_mut() {
			self.statement(stmt);
		}
	}

	fn item(&mut self, item: &mut Item, item_map: &mut ItemMap) {
		match item.0 {
			ItemType::Struct(id) => self.struct_decl(item_map.get_struct_mut(id)),
			ItemType::Template(id) => self.template(item_map.get_template_mut(id)),
			ItemType::Function(_, id) => self.function(item_map.get_function_mut(id)),
			ItemType::Enum(id) => self.enum_decl(item_map.get_enum_mut(id)),
		}
	}

	fn struct_decl(&mut self, s: &mut Struct) {
		for field in s.fields.iter_mut() {
			self.var_entry(field);
		}
	}

	fn enum_decl(&mut self, _e: &mut Enum) {}

	fn template(&mut self, t: &mut Template) {
		for arg in t.args.iter_mut() {
			self.var_entry(arg);
		}

		for stmt in t.block.iter_mut() {
			self.statement(stmt);
		}
	}

	fn expression(&mut self, expr: &mut ExpressionType) {
		match expr {
			ExpressionType::None => self.none(),
			ExpressionType::String(ref mut s) => self.string(s),
			ExpressionType::Number(ref mut n) => self.number(n),
			ExpressionType::Boolean(ref mut b) => self.boolean(b),
			ExpressionType::Block(ref mut block) => self.block(block),
			ExpressionType::Function(ref mut f) => self.function(f),
			ExpressionType::Code(ref mut block) => self.block(block),
			ExpressionType::Array(ref mut array) => self.array(array),
			ExpressionType::Map(ref mut map) => self.map(map),
			ExpressionType::Access(ref mut access) => self.access(access),
			ExpressionType::StructCreate(ref mut s) => self.struct_create(s),
			ExpressionType::RPNAccess(ref mut expr) => self.expression(&mut expr.0),
			ExpressionType::Index(ref mut index) => self.index(index),
			ExpressionType::Assignment(ref mut assignment) => self.assignment(assignment),
			ExpressionType::Unary(ref mut op, ref mut expr) => self.unary(op, expr),
			ExpressionType::Binary(ref mut left, ref mut op, ref mut right) => {
				self.binary(left.as_mut(), op, right.as_mut())
			},
			ExpressionType::Is(ref mut expr, ref mut ty) => self.is(expr, ty),
			ExpressionType::As(ref mut expr, ref mut ty) => self.as_expr(expr, ty),
			ExpressionType::Call(ref mut call) => self.call(call),
			ExpressionType::IfChain(ref mut chain) => self.if_chain(chain),
			ExpressionType::Switch(ref mut switch) => self.switch(switch),
			ExpressionType::While(ref mut while_loop) => self.while_loop(while_loop),
			ExpressionType::For(ref mut for_loop) => self.for_loop(for_loop),
			ExpressionType::Return(ref mut expr) => self.ret(expr.as_mut().map(|b| b.as_mut())),
			ExpressionType::Break(ref mut expr) => self.brek(expr.as_mut().map(|b| b.as_mut())),
			ExpressionType::Behavior(expr) => match expr {
				BehaviorExpression::Use(ref mut us) => self.template_use(us),
				BehaviorExpression::Component(ref mut component) => self.component(component),
				BehaviorExpression::Animation(ref mut animation) => self.animation(animation),
				BehaviorExpression::Visible(ref mut visible) => self.expression(&mut visible.0),
				BehaviorExpression::Emissive(ref mut emissive) => self.expression(&mut emissive.0),
				BehaviorExpression::Events(ref mut e1, ref mut e2) => self.events((e1.as_mut(), e2.as_mut())),
				BehaviorExpression::Interaction(ref mut interaction) => self.interaction(interaction),
				BehaviorExpression::Update(ref mut update) => self.update(update),
				BehaviorExpression::InputEvent(ref mut event) => self.input_event(event),
			},
		}
	}

	fn statement(&mut self, stmt: &mut Statement) {
		match stmt.0 {
			StatementType::Expression(ref mut expr) => self.expression(expr),
			StatementType::Declaration(ref mut var) => self.var(var),
		}
	}

	fn none(&mut self) {}

	fn string(&mut self, _s: &mut String) {}

	fn number(&mut self, _n: &mut f64) {}

	fn boolean(&mut self, _b: &mut bool) {}

	fn ty(&mut self, _ty: &mut Type) {}

	fn var_entry(&mut self, entry: &mut VarEntry) {
		self.ty(&mut entry.ty);
		if let Some(ref mut expr) = entry.default {
			self.expression(&mut expr.0);
		}
	}

	fn var(&mut self, var: &mut Variable) {
		if let Some(ref mut expr) = var.value {
			self.expression(&mut expr.0);
		}
	}

	fn struct_create(&mut self, create: &mut StructCreate) {
		self.ty(&mut create.ty);
		for value in create.values.iter_mut() {
			self.expression(&mut value.1 .0);
		}
	}

	fn block(&mut self, block: &mut Block) {
		for stmt in block.statements.iter_mut() {
			self.statement(stmt);
		}

		if let Some(ref mut expr) = block.expression {
			self.expression(&mut expr.0);
		}
	}

	fn array(&mut self, array: &mut Vec<Expression>) {
		for expr in array {
			self.expression(&mut expr.0);
		}
	}

	fn map(&mut self, map: &mut Vec<(Expression, Expression)>) {
		for expr in map {
			self.expression(&mut expr.0 .0);
			self.expression(&mut expr.1 .0);
		}
	}

	fn function(&mut self, f: &mut Function) {
		for arg in f.args.iter_mut() {
			self.var_entry(arg);
		}

		if let Some(ref mut ty) = f.ret {
			self.ty(ty);
		}

		self.block(&mut f.block);
	}

	fn access(&mut self, _access: &mut Access) {}

	fn index(&mut self, index: &mut Index) {
		self.expression(&mut index.array.0);
		self.expression(&mut index.index.0);
	}

	fn assignment(&mut self, assign: &mut Assignment) {
		match assign.target {
			AssignmentTarget::Var(ref mut access) => self.access(access),
			AssignmentTarget::RPNVar(ref mut rpn) => self.expression(&mut rpn.0),
			AssignmentTarget::Index(ref mut access, ref mut index) => {
				self.access(access);
				self.expression(&mut index.0);
			},
		};

		self.expression(&mut assign.value.0);
	}

	fn unary(&mut self, _op: &mut UnaryOperator, expr: &mut Expression) { self.expression(&mut expr.0); }

	fn binary(&mut self, left: &mut Expression, _op: &mut BinaryOperator, right: &mut Expression) {
		self.expression(&mut left.0);
		self.expression(&mut right.0);
	}

	fn is(&mut self, expr: &mut Expression, ty: &mut Type) {
		self.expression(&mut expr.0);
		self.ty(ty);
	}

	fn as_expr(&mut self, expr: &mut Expression, ty: &mut Type) {
		self.expression(&mut expr.0);
		self.ty(ty);
	}

	fn call(&mut self, call: &mut Call) {
		self.expression(&mut call.callee.0);
		for arg in call.args.iter_mut() {
			self.expression(&mut arg.0);
		}
	}

	fn if_chain(&mut self, chain: &mut IfChain) {
		for f in chain.ifs.iter_mut() {
			self.expression(&mut f.0 .0);
			self.block(&mut f.1);
		}

		if let Some(ref mut e) = chain.else_part {
			self.block(&mut e.0);
		}
	}

	fn switch(&mut self, switch: &mut Switch) {
		self.expression(&mut switch.on.0);
		for case in switch.cases.iter_mut() {
			self.expression(&mut case.value.0);
			self.expression(&mut case.code.0);
		}
	}

	fn for_loop(&mut self, _for_loop: &mut For) {}

	fn while_loop(&mut self, _while_loop: &mut While) {}

	fn ret(&mut self, expr: Option<&mut Expression>) { expr.map(|expr| self.expression(&mut expr.0)); }

	fn brek(&mut self, expr: Option<&mut Expression>) { expr.map(|expr| self.expression(&mut expr.0)); }

	fn template_use(&mut self, us: &mut Use) {
		for arg in us.args.iter_mut() {
			self.expression(&mut arg.1 .0);
		}
	}

	fn component(&mut self, component: &mut Component) {
		self.expression(&mut component.name.0);
		if let Some(ref mut node) = component.node {
			self.expression(&mut node.0);
		}
		for stmt in component.block.iter_mut() {
			self.statement(stmt);
		}
	}

	fn animation(&mut self, animation: &mut Animation) {
		self.expression(&mut animation.name.0);
		self.expression(&mut animation.length.0);
		self.expression(&mut animation.lag.0);
		self.expression(&mut animation.value.0);
	}

	fn interaction(&mut self, interaction: &mut Interaction) {
		self.expression(&mut interaction.legacy_cursors.0);
		self.expression(&mut interaction.lock_cursors.0);
		self.expression(&mut interaction.legacy_events.0);
		self.expression(&mut interaction.lock_events.0);
		self.expression(&mut interaction.legacy_callback.0);
		self.expression(&mut interaction.lock_events.0);
		self.expression(&mut interaction.lock_tooltip_title.0);
		self.expression(&mut interaction.lock_tooltips.0);
		self.expression(&mut interaction.animated_tooltips.0);
		self.expression(&mut interaction.cursor_animated.0);
		self.expression(&mut interaction.can_lock.0);
		interaction
			.node_to_highlight
			.as_mut()
			.map(|i| self.expression(&mut i.0));
		self.expression(&mut interaction.drag_mode.0);
	}

	fn update(&mut self, update: &mut Update) {
		update.frequency.as_mut().map(|f| self.expression(&mut f.0));
		self.expression(&mut update.mode.0);
		self.expression(&mut update.code.0);
	}

	fn events(&mut self, event: (&mut Expression, &mut Expression)) {
		self.expression(&mut event.0 .0);
		self.expression(&mut event.1 .0);
	}

	fn input_event(&mut self, event: &mut InputEvent) {
		self.expression(&mut event.name.0);
		self.expression(&mut event.legacy_icon.0);
		self.expression(&mut event.lock_icon.0);
		self.expression(&mut event.description.0);
		self.expression(&mut event.tooltip_value.0);
		self.expression(&mut event.legacy_interaction.0);
		self.expression(&mut event.lock_interaction.0);
		self.expression(&mut event.value.0);
		self.expression(&mut event.units.0);
		self.expression(&mut event.init.0);
		self.expression(&mut event.watch.0);
		self.expression(&mut event.inc.0);
		self.expression(&mut event.dec.0);
		self.expression(&mut event.set.0);
	}
}
