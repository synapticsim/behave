use std::collections::HashMap;

use diagnostic::Diagnostic;
use lexer::Lexer;
use parser::{Parser, ParserMode};

mod ast;
pub mod diagnostic;
mod evaluation;
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

#[derive(Debug)]
/// A `behave` source file.
pub struct SourceFile {
	/// The path of the source file.
	pub path: Vec<String>,
	/// The contents of the source file.
	pub contents: String,
}

/// Compile a `behave` project.
///
/// # Parameters:
/// `main_file`: The main file of the project.
/// `files`: Slice of all the other files in the project.
pub fn compile(main_file: &SourceFile, files: &[SourceFile]) -> CompileResult {
	let mut diagnostics = Vec::new();

	let main = match Parser::new(
		ParserMode::MainFile,
		&main_file.path,
		Lexer::new(&main_file.path, &main_file.contents),
		&mut diagnostics,
	)
	.parse()
	{
		Some(ast) => ast,
		None => {
			return CompileResult {
				compiled: None,
				diagnostics,
			}
		},
	};

	let mut asts = HashMap::new();
	for file in files {
		asts.insert(
			&file.path,
			match Parser::new(
				ParserMode::ImportedFile,
				&file.path,
				Lexer::new(&file.path, &file.contents),
				&mut diagnostics,
			)
			.parse()
			{
				Some(ast) => ast,
				None => {
					return CompileResult {
						compiled: None,
						diagnostics,
					}
				},
			},
		);
	}

	println!("{:#?}\n{:#?}", main, asts);

	CompileResult {
		compiled: None,
		diagnostics,
	}
}
