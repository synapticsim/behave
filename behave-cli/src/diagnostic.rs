use std::collections::HashMap;

use behave::diagnostic::{Diagnostic, LabelType, Level};
use behave::SourceFile;
use codespan_reporting::{
	diagnostic::{self, Label, LabelStyle, Severity},
	files::SimpleFiles,
	term::{
		self,
		termcolor::{ColorChoice, StandardStream},
		Config,
	},
};

pub fn display_diagnostics(file_map: Vec<SourceFile>, diagnostics: Vec<Diagnostic>) {
	let (files, id_map) = {
		let mut files = SimpleFiles::new();
		let mut id_map = HashMap::new();
		for file in file_map {
			id_map.insert(
				file.path.clone(),
				files.add(
					{
						let mut s = String::new();
						let mut iter = file.path.into_iter();
						s += &iter.next().unwrap();
						while let Some(p) = iter.next() {
							s.push('.');
							s += &p;
						}
						s
					},
					file.contents,
				),
			);
		}

		(files, id_map)
	};

	let mut writer = StandardStream::stderr(ColorChoice::Auto);
	let config = Config::default();

	for diagnostic in diagnostics {
		let diagnostic = diagnostic::Diagnostic {
			severity: match diagnostic.level {
				Level::Error => Severity::Error,
				Level::Warning => Severity::Warning,
				Level::Note => Severity::Note,
				Level::Help => Severity::Help,
			},
			code: None,
			message: diagnostic.message,
			labels: diagnostic
				.labels
				.into_iter()
				.map(|label| Label {
					style: match label.label_type {
						LabelType::Primary => LabelStyle::Primary,
						LabelType::Secondary => LabelStyle::Secondary,
					},
					file_id: id_map[&label.file],
					range: label.range,
					message: label.message,
				})
				.collect(),
			notes: diagnostic.notes,
		};

		term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
	}
}
