#![feature(try_trait_v2)]

use diagnostic::Diagnostic;
use lexer::Lexer;
use parser::{Parser, ParserMode};

use crate::ast::{ASTTree, ASTType, AST};
use crate::diagnostic::Level;
use crate::items::ItemMap;

mod ast;
pub mod diagnostic;
mod evaluation;
mod items;
mod lexer;
mod output;
mod parser;
mod resolve;
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
/// `loader`: File loader that loads files in the output directory for GLTF verification.
pub fn compile<F>(main_file: &SourceFile, files: &[SourceFile], loader: F) -> CompileResult
where
	F: FnMut(&str) -> Option<String>,
{
	let mut diagnostics = Vec::new();

	let mut item_map = ItemMap::new();
	let (mut main, mut others) = if let Ok(asts) = parse(main_file, files, &mut item_map, &mut diagnostics) {
		asts
	} else {
		return CompileResult {
			compiled: None,
			diagnostics,
		};
	};

	if let Err(diag) = resolve::resolve(&mut main, &mut others, &mut item_map) {
		diagnostics.extend(diag);
		return CompileResult {
			compiled: None,
			diagnostics,
		};
	}

	let (lods, behavior) = if let ASTType::Main(lods, behavior) = main.ast_data {
		(lods, behavior)
	} else {
		unreachable!()
	};

	let s = match output::generate(loader, &item_map, lods, behavior) {
		Ok(s) => Some(s),
		Err(err) => {
			diagnostics.extend(err);
			None
		},
	};

	CompileResult {
		compiled: s,
		diagnostics,
	}
}

fn parse<'a>(
	main_file: &'a SourceFile, files: &'a [SourceFile], item_map: &mut ItemMap<'a>, diagnostics: &mut Vec<Diagnostic>,
) -> Result<(AST<'a>, ASTTree<'a>), ()> {
	let main = match Parser::new(
		ParserMode::MainFile,
		&main_file.path,
		Lexer::new(&main_file.path, &main_file.contents),
		item_map,
		diagnostics,
	)
	.parse()
	{
		Some(ast) => ast,
		None => return Err(()),
	};

	let mut tree = ASTTree::new();
	for file in files {
		if !tree.add_ast(
			&file.path,
			match Parser::new(
				ParserMode::ImportedFile,
				&file.path,
				Lexer::new(&file.path, &file.contents),
				item_map,
				diagnostics,
			)
			.parse()
			{
				Some(ast) => ast,
				None => return Err(()),
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

	Ok((main, tree))
}
