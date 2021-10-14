use std::ops::Range;

pub struct Diagnostics {
	diagnostics: Vec<Diagnostic>,
	error: bool,
}

impl Diagnostics {
	pub fn new() -> Self {
		Self {
			diagnostics: Vec::new(),
			error: false,
		}
	}

	pub fn add(&mut self, diagnostic: Diagnostic) {
		if diagnostic.level == Level::Error {
			self.error = true;
		}
		self.diagnostics.push(diagnostic);
	}

	pub fn success(&self) -> bool { !self.error }

	pub fn get(self) -> Vec<Diagnostic> { self.diagnostics }
}

/// A location in source-code.
pub struct Location {
	pub span: Range<u32>,
	pub file: String,
}

impl Location {
	pub fn new(span: Range<u32>, file: &[impl AsRef<str>]) -> Self {
		Self {
			span,
			file: file.into_iter().map(|comp| comp.as_ref().to_string()).collect(),
		}
	}
}

/// The type of a label.
#[derive(Clone, Copy, PartialEq)]
pub enum LabelType {
	/// A primary label that shows the primary cause of a diagnostic.
	Primary,
	/// A secondary label that provides secondary information.
	Secondary,
}

/// A label in source code.
pub struct Label {
	/// The type of the label.
	pub ty: LabelType,
	/// The message associated with the label.
	pub message: String,
	/// The location of this diagnostic.
	pub loc: Location,
}

impl Label {
	pub fn primary(message: impl ToString, loc: Location) -> Self {
		Label {
			ty: LabelType::Primary,
			message: message.to_string(),
			loc,
		}
	}

	pub fn secondary(message: impl ToString, loc: Location) -> Self {
		Label {
			ty: LabelType::Secondary,
			message: message.to_string(),
			loc,
		}
	}
}

/// The level of a diagnostic.
#[derive(Clone, Copy, PartialEq)]
pub enum Level {
	Error,
	Warning,
	Note,
	Help,
}

/// A diagnostic.
pub struct Diagnostic {
	/// The diagnostic message.
	pub message: String,
	/// The leve; of the diagnostic.
	pub level: Level,
	/// The labels associated with the diagnostic.
	pub labels: Vec<Label>,
	/// Additional information.
	pub notes: Vec<String>,
}

impl Diagnostic {
	pub fn new(level: Level, message: impl ToString) -> Self {
		Self {
			message: message.to_string(),
			level,
			labels: Vec::new(),
			notes: Vec::new(),
		}
	}

	pub fn add_labels<I>(mut self, labels: I) -> Self
	where
		I: IntoIterator<Item = Label>,
	{
		self.labels.extend(labels);
		self
	}

	pub fn add_label(mut self, label: Label) -> Self {
		self.labels.push(label);
		self
	}

	pub fn add_notes<I, S>(mut self, notes: I) -> Self
	where
		I: IntoIterator<Item = S>,
		S: ToString,
	{
		self.notes.extend(notes.into_iter().map(|s| s.to_string()));
		self
	}

	pub fn add_note(mut self, note: impl ToString) -> Self {
		self.notes.push(note.to_string());
		self
	}
}
