use std::{collections::HashMap, io::Error};

use ast::{ImportType, AST};
use diagnostic::{Diagnostic, Level};
use lexer::Lexer;
use parser::{Parser, ParserMode};

use crate::diagnostic::Label;
use crate::ast::Path;

mod ast;
pub mod diagnostic;
mod lexer;
mod parser;
mod token;

/// The result of a compilation.
pub struct CompileResult {
	/// The compiled XML. Can be `None` if the compilation failed.
	pub compiled: Option<String>,
	/// The diagnostics generated during compilation.
	pub diagnostics: Vec<Diagnostic>,
}

/// Compile a `behave` file.
///
/// # Parameters:
/// `source_name`: The name of the source file.
/// `source`: The contents of the source file.
/// `import_resolver`: The resolver of imported files. It receives the import path, and should return the contents of
/// the imported file.
pub fn compile<F>(source_name: impl AsRef<str>, source: impl AsRef<str>, mut import_resolver: F) -> CompileResult
where
	F: FnMut(&str) -> Result<String, (String, Error)>,
{
	let mut diagnostics = Vec::new();
	let mut asts = HashMap::new();

	recursive_parse(
		source_name,
		source,
		ParserMode::MainFile,
		&mut import_resolver,
		&mut asts,
		&mut diagnostics,
	);

	println!("{:#?}", asts);

	CompileResult {
		compiled: None,
		diagnostics,
	}
}

fn recursive_parse<F>(
	source_name: impl AsRef<str>, source: impl AsRef<str>, mode: ParserMode, import_resolver: &mut F,
	asts: &mut HashMap<Vec<String>, AST>, diagnostics: &mut Vec<Diagnostic>,
) -> bool
where
	F: FnMut(&str) -> Result<String, (String, Error)>,
{
	let path = source_name_to_path(source_name.as_ref());
	if !asts.contains_key(&path) {
		if let Some(ast) = Parser::new(
			mode,
			source_name.as_ref(),
			Lexer::new(source_name.as_ref(), source.as_ref()),
			diagnostics,
		)
		.parse()
		{
			for import in &ast.imports {
				if let ImportType::Normal(path) = &import.0 {
					let path_string = path_to_source_name(path);

					match import_resolver(&path_string) {
						Ok(source) => {
							if !recursive_parse(
								&path_string,
								source,
								ParserMode::ImportedFile,
								import_resolver,
								asts,
								diagnostics,
							) {
								return false;
							}
						},
						Err(err) => {
							diagnostics.push(
								Diagnostic::new(Level::Error, format!("failed to import file `{}`: {}", err.0, err.1))
									.add_label(Label::primary(source_name.as_ref(), "imported here", path.1.clone())),
							);
							return false;
						},
					}
				}
			}

			asts.insert(path, ast);

			true
		} else {
			false
		}
	} else {
		true
	}
}

fn source_name_to_path(name: &str) -> Vec<String> {
	name.split('.').map(|s| s.to_string()).collect()
}

fn path_to_source_name(path: &Path) -> String {
	let mut s = String::new();
	let mut iter = path.0.iter().map(|i| &i.0);
	s += iter.next().unwrap();
	while let Some(p) = iter.next() {
		s += ".";
		s += p;
	}

	s
}
