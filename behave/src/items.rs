use crate::ast::{Enum, Function, Struct, Template};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StructId(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EnumId(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TemplateId(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FunctionId(usize);

pub struct ItemMap<'a> {
	structs: Vec<Struct<'a>>,
	enums: Vec<Enum<'a>>,
	templates: Vec<Template<'a>>,
	functions: Vec<Function<'a>>,
}

impl<'a> ItemMap<'a> {
	pub fn new() -> Self {
		Self {
			structs: Vec::new(),
			enums: Vec::new(),
			templates: Vec::new(),
			functions: Vec::new(),
		}
	}

	pub fn add_struct(&mut self, s: Struct<'a>) -> StructId {
		let id = self.structs.len();
		self.structs.push(s);
		StructId(id)
	}

	pub fn add_enum(&mut self, e: Enum<'a>) -> EnumId {
		let id = self.enums.len();
		self.enums.push(e);
		EnumId(id)
	}

	pub fn add_template(&mut self, t: Template<'a>) -> TemplateId {
		let id = self.templates.len();
		self.templates.push(t);
		TemplateId(id)
	}

	pub fn add_function(&mut self, f: Function<'a>) -> FunctionId {
		let id = self.functions.len();
		self.functions.push(f);
		FunctionId(id)
	}

	pub fn get_struct(&self, id: StructId) -> &Struct<'a> { &self.structs[id.0] }

	pub fn get_struct_mut(&mut self, id: StructId) -> &mut Struct<'a> { &mut self.structs[id.0] }

	pub fn get_enum(&self, id: EnumId) -> &Enum<'a> { &self.enums[id.0] }

	pub fn get_enum_mut(&mut self, id: EnumId) -> &mut Enum<'a> { &mut self.enums[id.0] }

	pub fn get_template(&self, id: TemplateId) -> &Template<'a> { &self.templates[id.0] }

	pub fn get_template_mut(&mut self, id: TemplateId) -> &mut Template<'a> { &mut self.templates[id.0] }

	pub fn get_function(&self, id: FunctionId) -> &Function<'a> { &self.functions[id.0] }

	pub fn get_function_mut(&mut self, id: FunctionId) -> &mut Function<'a> { &mut self.functions[id.0] }
}
