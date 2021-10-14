use std::{
	fs::{read_to_string, write},
	panic::set_hook,
	path::PathBuf,
};

use behave::{
	diagnostic::{Diagnostic, Level},
	SourceFile,
};
use clap::{crate_version, AppSettings, Clap};
use diagnostic::display_diagnostics;

mod diagnostic;

/// A compiler for Microsoft Flight Simulator ModelBehaviors and ModelInfo.
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
	set_hook(Box::new(|info| {
		println!("Internal Compiler Error: This is a bug, please report it to https://github.com/Synaptic-Simulations/behave/issues");
		println!("{}", info);
	}));

	let options = Options::parse();
	let result = compile(&options);
	display_diagnostics(result.files, result.diagnostics);
}

fn compile(options: &Options) -> CompileResult {
	let file_contents = match read_to_string(&options.file) {
		Ok(s) => s,
		Err(err) => {
			return CompileResult {
				files: Vec::new(),
				diagnostics: vec![Diagnostic::new(
					Level::Error,
					format!("failed to open file '{}': {}", options.file.display(), err),
				)],
			}
		},
	};

	let file_name = options.file.file_name().unwrap().to_string_lossy().into();
	let main_file = SourceFile {
		path: file_name,
		contents: file_contents,
	};
	let mut files = Vec::new();
	let file_root = options.file.parent().unwrap();
	let output_root = options.output.parent().unwrap();

	let result = behave::compile(
		&main_file,
		|file| {
			let mut buf = file_root.to_path_buf();
			buf.extend(file);
			buf.set_extension("beh");
			read_to_string(&buf).ok().map(|contents| {
				let source = Box::new(SourceFile {
					path: buf.to_string_lossy().into(),
					contents,
				});
				let source_ref = source.as_ref() as *const SourceFile;
				files.push(source);
				unsafe { &*source_ref }
			})
		},
		|file| {
			let mut path = output_root.to_path_buf();
			path.push(file);
			read_to_string(path).ok()
		},
	);
	let files = files
		.into_iter()
		.map(|source| *source)
		.chain(std::iter::once(main_file))
		.collect();

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

	CompileResult {
		files,
		diagnostics: result.diagnostics,
	}
}
