use std::collections::HashSet;

use gltf::json::Root;
use uuid::Uuid;

use crate::ast::{Behavior, Block, LODs, Location};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::items::ItemMap;
use crate::output::xml::XMLWriter;
use crate::runtime::expression::ExpressionEvaluator;
use crate::runtime::value::{RuntimeAnimation, RuntimeComponent, TemplateValue};

mod xml;

macro_rules! get {
	($expr:expr, $errors:ident) => {
		match $expr {
			Ok(val) => val,
			Err(err) => {
				$errors.extend(err);
				continue;
			},
		}
	};
}

struct GLTFData<'a> {
	nodes: HashSet<String>,
	animations: HashSet<String>,
	loc: Location<'a>,
}

pub fn generate<F>(loader: F, item_map: &ItemMap, lods: LODs, behavior: Behavior) -> Result<String, Vec<Diagnostic>>
where
	F: FnMut(&str) -> Option<String>,
{
	let mut evaluator = ExpressionEvaluator::new(item_map);
	let mut writer = XMLWriter::start();
	let mut errors = Vec::new();
	let mut gltfs = Vec::new();

	generate_lods(loader, lods, &mut gltfs, &mut writer, &mut evaluator, &mut errors);
	generate_behavior(behavior, item_map, &gltfs, &mut writer, &mut evaluator, &mut errors);

	if errors.len() == 0 {
		Ok(writer.end())
	} else {
		Err(errors)
	}
}

fn generate_lods<'a, F>(
	mut loader: F, lods: LODs<'a>, gltfs: &mut Vec<GLTFData<'a>>, writer: &mut XMLWriter,
	evaluator: &mut ExpressionEvaluator<'a>, errors: &mut Vec<Diagnostic>,
) where
	F: FnMut(&str) -> Option<String>,
{
	writer.start_element("LODS");
	for lod in lods.0 {
		let min_size = get!(
			evaluator.evaluate_as_number(&lod.min_size, "LOD minimum size must be a number"),
			errors
		);
		let file = get!(
			evaluator.evaluate_as_string(&lod.file, "LOD file must be a string"),
			errors
		);

		if let Some(data) = loader(&file) {
			if let Ok(root) = Root::from_str(&data) {
				let mut nodes = HashSet::new();
				for node in root.nodes {
					node.name.map(|s| nodes.insert(s));
				}
				let mut animations = HashSet::new();
				for animation in root.animations {
					animation.name.map(|s| animations.insert(s));
				}
				gltfs.push(GLTFData {
					nodes,
					animations,
					loc: lod.loc,
				});
			} else {
				errors.push(
					Diagnostic::new(Level::Error, "LOD file is not valid").add_label(Label::primary(
						format!("the file `{}` is not valid GLTF", file),
						lod.file.1.clone(),
					)),
				);
			}
		} else {
			errors.push(
				Diagnostic::new(Level::Error, "LOD file does not exist").add_label(Label::primary(
					format!("the file `{}` does not exist", file),
					lod.file.1.clone(),
				)),
			);
			continue;
		}

		writer.element(
			"LOD",
			[("MinSize", min_size.to_string()), ("ModelFile", file)].into_iter(),
		)
	}
	writer.end_element();
}

fn generate_behavior<'a>(
	behavior: Behavior<'a>, item_map: &ItemMap<'a>, gltfs: &[GLTFData], writer: &mut XMLWriter,
	evaluator: &mut ExpressionEvaluator<'a>, errors: &mut Vec<Diagnostic>,
) {
	let values = match evaluator.evaluate_behavior(&behavior) {
		Ok(v) => v,
		Err(err) => {
			errors.extend(err);
			return;
		},
	};

	writer.start_element("Behaviors");
	generate_template_values(values, gltfs, writer, errors);
	writer.end_element();
}

fn generate_template_values<'a>(
	values: Vec<TemplateValue<'a>>, gltfs: &[GLTFData], writer: &mut XMLWriter, errors: &mut Vec<Diagnostic>,
) {
	for value in values {
		match value {
			TemplateValue::Component(component) => generate_component(component, gltfs, writer, errors),
			TemplateValue::Animation(animation) => generate_animation(animation, gltfs, writer, errors),
			TemplateValue::Visibility(condition) => generate_visibility(condition, writer, errors),
			TemplateValue::Block(values) => generate_template_values(values, gltfs, writer, errors),
		}
	}
}

fn generate_component(
	component: RuntimeComponent, gltfs: &[GLTFData], writer: &mut XMLWriter, errors: &mut Vec<Diagnostic>,
) {
	if let Some(node) = component.node {
		let mut lods_without = Vec::new();
		for gltf in gltfs.iter().enumerate() {
			if !gltf.1.nodes.contains(&node.0) {
				lods_without.push(gltf.0);
			}
		}

		if lods_without.len() == gltfs.len() {
			errors.push(
				Diagnostic::new(Level::Error, "node does not exist").add_label(Label::primary(
					format!("node `{}` does not exist in any LOD", node.0),
					node.1.clone(),
				)),
			)
		} else {
			for lod in lods_without {
				errors.push(
					Diagnostic::new(Level::Warning, "LOD does not have node")
						.add_label(Label::primary(
							format!("node `{}` does not exist in LOD", node.0),
							gltfs[lod].loc.clone(),
						))
						.add_label(Label::secondary("node was defined here", node.1.clone())),
				)
			}
		}

		writer.start_element_attrib("Component", [("ID", component.name), ("Node", node.0)]);
	} else {
		writer.start_element_attrib("Component", [("ID", component.name)]);
	}

	generate_template_values(component.items, gltfs, writer, errors);

	writer.end_element();
}

fn generate_animation(
	animation: RuntimeAnimation, gltfs: &[GLTFData], writer: &mut XMLWriter, errors: &mut Vec<Diagnostic>,
) {
	let mut lods_without = Vec::new();
	for gltf in gltfs.iter().enumerate() {
		if !gltf.1.animations.contains(&animation.name.0) {
			lods_without.push(gltf.0);
		}
	}

	if lods_without.len() == gltfs.len() {
		errors.push(
			Diagnostic::new(Level::Error, "animation does not exist").add_label(Label::primary(
				format!("animation `{}` does not exist in any LOD", animation.name.0),
				animation.name.1.clone(),
			)),
		)
	} else {
		for lod in lods_without {
			errors.push(
				Diagnostic::new(Level::Warning, "LOD does not have animation")
					.add_label(Label::primary(
						format!("animation `{}` does not exist in LOD", animation.name.0),
						gltfs[lod].loc.clone(),
					))
					.add_label(Label::secondary("node was defined here", animation.name.1.clone())),
			)
		}
	}

	writer.start_element_attrib(
		"Animation",
		[
			("Name", animation.name.0),
			("Guid", format!("{{{}}}", Uuid::new_v4().to_hyphenated())),
			("Length", animation.length.to_string()),
			("Type", "Sim".to_string()),
			("TypeParam", "AutoPlay".to_string()),
		],
	);
	writer.start_element("Parameters");
	writer.start_element("Lag");
	writer.data(animation.lag.to_string());
	writer.end_element();
	writer.start_element("Code");
	// Compiled RPN goes here
	writer.end_element();
	writer.end_element();
	writer.end_element();
}

fn generate_visibility(condition: Block, writer: &mut XMLWriter, errors: &mut Vec<Diagnostic>) {
	writer.start_element("Visibility");
	writer.start_element("Parameter");
	writer.start_element("Code");
	// Compiled RPN goes here
	writer.end_element();
	writer.end_element();
	writer.end_element();
}
