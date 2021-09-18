use diagnostic::{Diagnostic, Level};
use lexer::Lexer;
use parser::{Parser, ParserMode};

use crate::ast::ASTTree;
use crate::evaluation::evaluate;

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

	let mut tree = ASTTree::new();
	for file in files {
		if !tree.add_ast(
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
		) {
			diagnostics.push(Diagnostic::new(
				Level::Error,
				format!("file '{}' is invalid", {
					let mut s = String::new();
					let mut iter = file.path.iter();
					s += &iter.next().unwrap();
					while let Some(p) = iter.next() {
						s.push('.');
						s += &p;
					}
					s
				}),
			))
		}
	}

	CompileResult {
		compiled: evaluate(&main_file.path, main, tree),
		diagnostics,
	}
}
