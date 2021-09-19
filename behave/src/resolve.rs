use std::collections::HashMap;

use crate::ast::{
	ASTTree,
	ASTType,
	Access,
	Animation,
	Assignment,
	AssignmentTarget,
	Block,
	Call,
	Component,
	Expression,
	ExpressionType,
	For,
	Function,
	IfChain,
	ImportType,
	Index,
	ItemType,
	Path,
	ResolvedType,
	Statement,
	StatementType,
	Switch,
	TypeType,
	Use,
	UserType,
	While,
	AST,
};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::items::{ItemMap, TemplateId};

#[derive(Debug)]
struct Resolver<'a> {
	types: HashMap<Vec<String>, ResolvedType>,
	templates: HashMap<Vec<String>, TemplateId>,
	diagnostics: &'a mut Vec<Diagnostic>,
}

impl<'a> Resolver<'a> {
	fn new<'b>(
		diagnostics: &'a mut Vec<Diagnostic>, this: &'b AST<'b>, root_tree: &'b ASTTree<'b>, item_map: &'b ItemMap<'b>,
		imports: impl Iterator<Item = &'b Path<'b>>,
	) -> Result<Resolver<'a>, ()> {
		let mut roots = vec![root_tree];
		for import in imports {
			match root_tree.get_ast(&import.0) {
				Ok(tree) => roots.push(tree),
				Err(diag) => diagnostics.push(diag.add_label(Label::primary("here", import.1.clone()))),
			}
		}

		if diagnostics.len() == 0 {
			let mut se = Self {
				types: HashMap::new(),
				templates: HashMap::new(),
				diagnostics,
			};

			for root in roots {
				se.add_types_from_root(root, item_map, &[]);
			}

			if let ASTType::Secondary(ref items) = this.ast_data {
				for item in items {
					match item.0 {
						ItemType::Enum(e) => {
							let en = item_map.get_enum(e);
							se.types.insert(vec![en.name.0.clone()], ResolvedType::Enum(e));
						},
						ItemType::Struct(s) => {
							let st = item_map.get_struct(s);
							se.types.insert(vec![st.name.0.clone()], ResolvedType::Struct(s));
						},
						ItemType::Template(t) => {
							let te = item_map.get_template(t);
							se.templates.insert(vec![te.name.0.clone()], t);
						},
						_ => {},
					}
				}
			}

			Ok(se)
		} else {
			Err(())
		}
	}

	fn add_types_from_root<'b>(&mut self, root: &'b ASTTree<'b>, item_map: &'b ItemMap<'b>, path: &[String]) {
		match root {
			ASTTree::Branch(ref map) => {
				for pair in map {
					let mut path = path.to_vec();
					path.push(pair.0.clone());
					self.add_types_from_root(pair.1, item_map, &path);
				}
			},
			ASTTree::Leaf(ref ast) => {
				if let ASTType::Secondary(ref items) = ast.ast_data {
					for item in items {
						match item.0 {
							ItemType::Enum(e) => {
								let mut path = path.to_vec();
								let en = item_map.get_enum(e);
								path.push(en.name.0.clone());
								self.types.insert(path, ResolvedType::Enum(e));
							},
							ItemType::Struct(s) => {
								let mut path = path.to_vec();
								let st = item_map.get_struct(s);
								path.push(st.name.0.clone());
								self.types.insert(path, ResolvedType::Struct(s));
							},
							ItemType::Template(t) => {
								let mut path = path.to_vec();
								let te = item_map.get_template(t);
								path.push(te.name.0.clone());
								self.templates.insert(path, t);
							},
							_ => {},
						}
					}
				} else {
					unreachable!()
				}
			},
		}
	}

	fn resolve_expression<'b>(&mut self, expr: &mut ExpressionType<'b>) {
		match expr {
			ExpressionType::Block(ref mut block) => self.resolve_block(block),
			ExpressionType::Function(ref mut f) => self.resolve_function(f),
			ExpressionType::Code(ref mut block) => self.resolve_block(block),
			ExpressionType::Array(ref mut array) => self.resolve_array(array),
			ExpressionType::Access(ref mut access) => self.resolve_access(access),
			ExpressionType::StructCreate(ref mut s) => self.resolve_type(&mut s.ty),
			ExpressionType::RPNAccess(ref mut expr) => self.resolve_expression(&mut expr.0),
			ExpressionType::Index(ref mut index) => self.resolve_index(index),
			ExpressionType::Assignment(ref mut assignment) => self.resolve_assignment(assignment),
			ExpressionType::Unary(_, ref mut expr) => self.resolve_expression(&mut expr.0),
			ExpressionType::Binary(ref mut left, _, ref mut right) => {
				self.resolve_expression(&mut left.0);
				self.resolve_expression(&mut right.0);
			},
			ExpressionType::Call(ref mut call) => self.resolve_call(call),
			ExpressionType::IfChain(ref mut chain) => self.resolve_if(chain),
			ExpressionType::Switch(ref mut switch) => self.resolve_switch(switch),
			ExpressionType::While(ref mut while_loop) => self.resolve_while(while_loop),
			ExpressionType::For(ref mut for_loop) => self.resolve_for(for_loop),
			ExpressionType::Return(ref mut expr) => {
				if let Some(expr) = expr {
					self.resolve_expression(&mut expr.0)
				}
			},
			ExpressionType::Break(ref mut expr) => {
				if let Some(expr) = expr {
					self.resolve_expression(&mut expr.0)
				}
			},
			ExpressionType::Use(ref mut us) => self.resolve_use(us),
			ExpressionType::Component(ref mut component) => self.resolve_component(component),
			ExpressionType::Animation(ref mut animation) => self.resolve_animation(animation),
			_ => {},
		}
	}

	fn resolve_statement<'b>(&mut self, stmt: &mut Statement<'b>) {
		match stmt.0 {
			StatementType::Expression(ref mut expr) => self.resolve_expression(expr),
			_ => {},
		}
	}

	fn resolve_block<'b>(&mut self, block: &mut Block<'b>) {
		for stmt in block.statements.iter_mut() {
			self.resolve_statement(stmt);
		}

		if let Some(ref mut expr) = block.expression {
			self.resolve_expression(&mut expr.0);
		}
	}

	fn resolve_array<'b>(&mut self, array: &mut Vec<Expression<'b>>) {
		for expr in array {
			self.resolve_expression(&mut expr.0);
		}
	}

	fn resolve_function<'b>(&mut self, f: &mut Function<'b>) {
		for arg in f.args.iter_mut() {
			if let TypeType::User(ref mut u) = arg.ty.0 {
				self.resolve_type(u);
			}
		}

		if let Some(ref mut ty) = f.ret {
			if let TypeType::User(ref mut u) = ty.0 {
				self.resolve_type(u);
			}
		}

		self.resolve_block(&mut f.block);
	}

	fn resolve_access<'b>(&self, access: &mut Access<'b>) {}

	fn resolve_index<'b>(&mut self, index: &mut Index<'b>) {
		self.resolve_expression(&mut index.array.0);
		self.resolve_expression(&mut index.index.0);
	}

	fn resolve_assignment<'b>(&mut self, assign: &mut Assignment<'b>) {
		match assign.target {
			AssignmentTarget::Var(ref mut access) => self.resolve_access(access),
			AssignmentTarget::RPNVar(ref mut rpn) => self.resolve_expression(&mut rpn.0),
			AssignmentTarget::Index(ref mut access, ref mut index) => {
				self.resolve_access(access);
				self.resolve_expression(&mut index.0);
			},
		};

		self.resolve_expression(&mut assign.value.0);
	}

	fn resolve_call<'b>(&mut self, call: &mut Call<'b>) {
		self.resolve_expression(&mut call.callee.0);
		for arg in call.args.iter_mut() {
			self.resolve_expression(&mut arg.0);
		}
	}

	fn resolve_if<'b>(&mut self, chain: &mut IfChain<'b>) {
		for f in chain.ifs.iter_mut() {
			self.resolve_expression(&mut f.0 .0);
			self.resolve_block(&mut f.1);
		}

		if let Some(ref mut e) = chain.else_part {
			self.resolve_block(&mut e.0);
		}
	}

	fn resolve_switch<'b>(&mut self, switch: &mut Switch<'b>) {
		self.resolve_expression(&mut switch.on.0);
		for case in switch.cases.iter_mut() {
			self.resolve_expression(&mut case.value.0);
			self.resolve_expression(&mut case.code.0);
		}
	}

	fn resolve_for<'b>(&mut self, for_loop: &mut For<'b>) {}

	fn resolve_while<'b>(&mut self, while_loop: &mut While<'b>) {}

	fn resolve_type<'b>(&mut self, ty: &mut UserType<'b>) {
		if let Some(resolved) = self
			.types
			.get(&ty.path.0.iter().map(|s| s.0.clone()).collect::<Vec<_>>())
		{
			ty.resolved = Some(*resolved);
		} else {
			self.diagnostics.push(
				Diagnostic::new(Level::Error, "type does not exist")
					.add_label(Label::primary("here", ty.path.1.clone())),
			)
		}
	}

	fn resolve_use<'b>(&mut self, us: &mut Use<'b>) {
		if let Some(resolved) = self
			.templates
			.get(&us.template.path.0.iter().map(|s| s.0.clone()).collect::<Vec<_>>())
		{
			us.template.resolved = Some(*resolved);
		} else {
			self.diagnostics.push(
				Diagnostic::new(Level::Error, "template does not exist")
					.add_label(Label::primary("here", us.template.path.1.clone())),
			)
		}
	}

	fn resolve_component<'b>(&mut self, component: &mut Component<'b>) {
		self.resolve_expression(&mut component.name.0);
		if let Some(ref mut node) = component.node {
			self.resolve_expression(&mut node.0);
		}
		for stmt in component.block.iter_mut() {
			self.resolve_statement(stmt);
		}
	}

	fn resolve_animation<'b>(&mut self, animation: &mut Animation<'b>) {
		self.resolve_expression(&mut animation.name.0);
		self.resolve_expression(&mut animation.length.0);
		self.resolve_expression(&mut animation.lag.0);
		self.resolve_expression(&mut animation.code.0);
	}
}

pub fn resolve(main: &mut AST, others: &mut ASTTree, item_map: &mut ItemMap) -> Result<(), Vec<Diagnostic>> {
	let mut errors = Vec::new();

	let copied_for_imported = others.clone();
	if let Err(diag) = resolve_imported(&copied_for_imported, others, item_map) {
		errors.extend(diag);
	}

	let imports = main.imports.iter().filter_map(|import| {
		if let ImportType::Normal(p) = &import.0 {
			Some(p)
		} else {
			None
		}
	});
	let mut resolver = if let Ok(res) = Resolver::new(&mut errors, main, others, item_map, imports) {
		res
	} else {
		return Err(errors);
	};

	if let ASTType::Main(ref mut lods, ref mut behavior) = main.ast_data {
		for lod in lods.0.iter_mut() {
			resolver.resolve_expression(&mut lod.min_size.0);
			resolver.resolve_expression(&mut lod.file.0);
		}

		for stmt in behavior.0.iter_mut() {
			resolver.resolve_statement(stmt);
		}
	} else {
		unreachable!();
	}

	if errors.len() == 0 {
		Ok(())
	} else {
		Err(errors)
	}
}

fn resolve_imported(root: &ASTTree, tree: &mut ASTTree, item_map: &mut ItemMap) -> Result<(), Vec<Diagnostic>> {
	let mut errors = Vec::new();

	match tree {
		ASTTree::Branch(ref mut map) => {
			for pair in map.iter_mut() {
				if let Err(diag) = resolve_imported(root, pair.1, item_map) {
					errors.extend(diag);
				}
			}
		},
		ASTTree::Leaf(ref mut ast) => {
			let mut resolver = if let Ok(res) = Resolver::new(
				&mut errors,
				ast,
				root,
				item_map,
				ast.imports.iter().filter_map(|import| {
					if let ImportType::Normal(p) = &import.0 {
						Some(p)
					} else {
						None
					}
				}),
			) {
				res
			} else {
				return Err(errors);
			};

			if let ASTType::Secondary(ref mut items) = ast.ast_data {
				for item in items {
					match item.0 {
						ItemType::Struct(id) => {
							let s = item_map.get_struct_mut(id);
							for field in s.fields.iter_mut() {
								if let TypeType::User(ref mut us) = field.ty.0 {
									resolver.resolve_type(us);
								}

								if let Some(ref mut expr) = field.default {
									resolver.resolve_expression(&mut expr.0);
								}
							}
						},
						ItemType::Template(id) => {
							let t = item_map.get_template_mut(id);

							for arg in t.args.iter_mut() {
								if let TypeType::User(ref mut us) = arg.ty.0 {
									resolver.resolve_type(us);
								}

								if let Some(ref mut expr) = arg.default {
									resolver.resolve_expression(&mut expr.0);
								}
							}

							for stmt in t.block.iter_mut() {
								resolver.resolve_statement(stmt);
							}
						},
						ItemType::Function(_, ref mut f) => resolver.resolve_function(f),
						_ => {},
					}
				}
			} else {
				unreachable!()
			}
		},
	}

	if errors.len() == 0 {
		Ok(())
	} else {
		Err(errors)
	}
}
