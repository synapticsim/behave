use std::collections::HashMap;

use crate::ast::{
	ASTPass,
	ASTTree,
	ASTType,
	Access,
	EnumAccess,
	GlobalAccess,
	ImportType,
	ItemType,
	Path,
	ResolvedAccess,
	ResolvedType,
	Type,
	TypeType,
	Use,
	AST,
};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::items::{FunctionId, ItemMap, TemplateId};

#[derive(Debug)]
struct Resolver<'a> {
	types: HashMap<Vec<String>, ResolvedType>,
	templates: HashMap<Vec<String>, TemplateId>,
	functions: HashMap<Vec<String>, FunctionId>,
	enum_variants: HashMap<Vec<String>, EnumAccess>,
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
			let mut resolver = Self {
				types: HashMap::new(),
				templates: HashMap::new(),
				functions: HashMap::new(),
				enum_variants: HashMap::new(),
				diagnostics,
			};

			for root in roots {
				resolver.add_types_from_root(root, item_map, &[]);
			}

			if let ASTType::Secondary(ref items) = this.ast_data {
				for item in items {
					match item.0 {
						ItemType::Enum(e) => {
							let en = item_map.get_enum(e);
							resolver.types.insert(vec![en.name.0.clone()], ResolvedType::Enum(e));

							for variant in en.variants.iter() {
								let mut path = vec![en.name.0.clone()];
								path.push(variant.name.0.clone());
								resolver.enum_variants.insert(
									path,
									EnumAccess {
										id: e,
										value: variant.value,
									},
								);
							}
						},
						ItemType::Struct(s) => {
							let st = item_map.get_struct(s);
							resolver.types.insert(vec![st.name.0.clone()], ResolvedType::Struct(s));
						},
						ItemType::Template(t) => {
							let te = item_map.get_template(t);
							resolver.templates.insert(vec![te.name.0.clone()], t);
						},
						ItemType::Function(ref name, f) => {
							resolver.functions.insert(vec![name.0.clone()], f);
						},
					}
				}
			}

			Ok(resolver)
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
								self.types.insert(path.clone(), ResolvedType::Enum(e));

								for variant in en.variants.iter() {
									let mut path = path.clone();
									path.push(variant.name.0.clone());
									self.enum_variants.insert(
										path,
										EnumAccess {
											id: e,
											value: variant.value,
										},
									);
								}
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
							ItemType::Function(ref name, f) => {
								let mut path = path.to_vec();
								path.push(name.0.clone());
								self.functions.insert(path, f);
							},
						}
					}
				} else {
					unreachable!()
				}
			},
		}
	}
}

impl ASTPass for Resolver<'_> {
	fn ty<'b>(&mut self, ty: &mut Type<'b>) {
		if let TypeType::User(ref mut user) = ty.0 {
			if let Some(resolved) = self
				.types
				.get(&user.path.0.iter().map(|s| s.0.clone()).collect::<Vec<_>>())
			{
				user.resolved = Some(*resolved);
			} else {
				self.diagnostics.push(
					Diagnostic::new(Level::Error, "type does not exist")
						.add_label(Label::primary("here", user.path.1.clone())),
				)
			}
		}
	}

	fn access(&mut self, access: &mut Access) {
		access.resolved = Some(
			if let Some(resolved) = self
				.functions
				.get(&access.path.0.iter().map(|s| s.0.clone()).collect::<Vec<_>>())
			{
				ResolvedAccess::Global(GlobalAccess::Function(*resolved))
			} else if let Some(resolved) = self
				.enum_variants
				.get(&access.path.0.iter().map(|s| s.0.clone()).collect::<Vec<_>>())
			{
				ResolvedAccess::Global(GlobalAccess::Enum(*resolved))
			} else {
				ResolvedAccess::Local
			},
		);
	}

	fn template_use<'b>(&mut self, us: &mut Use<'b>) {
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

		for arg in us.args.iter_mut() {
			self.expression(&mut arg.1 .0);
		}
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
		resolver.lods(lods);
		resolver.behavior(behavior);
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
					resolver.item(item, item_map)
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
