use crate::{
	diagnostic::{Diagnostic, Diagnostics},
	syntax::parser::Parser,
};

pub mod diagnostic;
// mod output;
pub mod syntax;

/// The result of a compilation.
pub struct CompileResult {
	/// The compiled XML. Can be `None` if the compilation failed.
	pub compiled: Option<String>,
	/// The diagnostics generated during compilation.
	pub diagnostics: Vec<Diagnostic>,
}

#[derive(Clone)]
/// A `behave` source file.
pub struct SourceFile {
	/// The path of the source file.
	pub path: String,
	/// The contents of the source file.
	pub contents: String,
}

/// Compile a `behave` project.
///
/// # Parameters:
/// `file`: The main file of the project.
/// `source_loader`: File loader that loads files relative to the main file of the project.
/// `out_loader`: File loader that loads files in the output directory for GLTF verification.
pub fn compile<'a, S, O>(file: &'a SourceFile, mut source_loader: S, out_loader: O) -> CompileResult
where
	S: FnMut(&[&str]) -> Option<&'a SourceFile>,
	O: FnMut(&str) -> Option<String>,
{
	let mut diagnostics = Diagnostics::new();

	let ast = Parser::new(file, &[], &mut source_loader, &mut diagnostics).main();
	println!("{:#?}", ast);

	CompileResult {
		compiled: None,
		diagnostics: diagnostics.get(),
	}
}
