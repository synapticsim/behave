use std::fs::read_dir;
use std::path::Path;
use std::{
	fs::{read_to_string, write},
	path::PathBuf,
};

use behave::diagnostic::{Diagnostic, Level};
use behave::SourceFile;
use clap::{crate_version, AppSettings, Clap};
use diagnostic::display_diagnostics;

mod diagnostic;

/// A compiler for Microsoft Flight Simulator ModelBehaviors and ModelInfo.
/// It also compiles to RPN.
#[derive(Clap)]
#[clap(version = crate_version!(), author = "Synaptic Simulations")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Options {
	/// The `behave` file to compile.
	file: PathBuf,
	/// The output path of the compiled XML.
	#[clap(short, long)]
	output: PathBuf,
}

struct CompileResult {
	pub files: Vec<SourceFile>,
	pub diagnostics: Vec<Diagnostic>,
}

fn main() {
	let options = Options::parse();
	let result = compile(&options);
	display_diagnostics(result.files, result.diagnostics);
}

fn compile(options: &Options) -> CompileResult {
	let mut files = Vec::new();
	let mut diagnostics = Vec::new();

	let main_file_content = match read_to_string(&options.file) {
		Ok(s) => s,
		Err(err) => {
			return CompileResult {
				files,
				diagnostics: vec![Diagnostic::new(
					Level::Error,
					format!("failed to open file '{}': {}", options.file.display(), err),
				)],
			}
		},
	};
	let file_name = vec![options.file.file_stem().unwrap().to_string_lossy().into()];
	let main_file = SourceFile {
		path: file_name,
		contents: main_file_content,
	};

	let file_root = options.file.parent().unwrap();
	open_files(&options.file, file_root, &mut files, &mut diagnostics, &Vec::new());
	if diagnostics.len() > 0 {
		return CompileResult { files, diagnostics };
	}

	let result = behave::compile(&main_file, &files);
	diagnostics.extend(result.diagnostics);
	files.push(main_file);

	if let Some(output) = result.compiled {
		match write(&options.output, output) {
			Err(err) => {
				return CompileResult {
					files,
					diagnostics: vec![Diagnostic::new(
						Level::Error,
						format!("failed to write to file '{}': {}", options.output.display(), err),
					)],
				}
			},
			_ => {},
		};
	}

	CompileResult { files, diagnostics }
}

fn open_files(
	main_file: &Path, dir: &Path, files: &mut Vec<SourceFile>, diagnostics: &mut Vec<Diagnostic>, path: &Vec<String>,
) {
	let dir_iter = match read_dir(dir) {
		Ok(dir) => dir,
		Err(err) => {
			diagnostics.push(Diagnostic::new(
				Level::Error,
				format!("Failed to open directory '{}': {}", dir.display(), err),
			));
			return;
		},
	};

	for entry in dir_iter {
		let entry = match entry {
			Ok(entry) => entry,
			Err(err) => {
				diagnostics.push(Diagnostic::new(
					Level::Error,
					format!("failed to open directory '{}': {}", dir.display(), err),
				));
				return;
			},
		};

		let metadata = match entry.metadata() {
			Ok(metadata) => metadata,
			Err(err) => {
				diagnostics.push(Diagnostic::new(
					Level::Error,
					format!("failed to open file '{}': {}", entry.path().display(), err),
				));
				return;
			},
		};

		if metadata.is_file()
			&& entry.path().extension().map(|ext| ext == "beh").unwrap_or(false)
			&& main_file != entry.path()
		{
			let contents = match read_to_string(entry.path()) {
				Ok(s) => s,
				Err(err) => {
					diagnostics.push(Diagnostic::new(
						Level::Error,
						format!("failed to open file '{}': {}", entry.path().display(), err),
					));
					return;
				},
			};

			let path = {
				let mut path = path.clone();
				path.push(entry.path().file_stem().unwrap().to_string_lossy().into());
				path
			};

			files.push(SourceFile { path, contents })
		} else if metadata.is_dir() {
			open_files(main_file, &entry.path(), files, diagnostics, &{
				let mut path = path.clone();
				path.push(entry.path().to_string_lossy().into());
				path
			})
		}
	}
}
