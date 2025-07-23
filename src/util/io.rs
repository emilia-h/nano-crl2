
use crate::core::diagnostic::{Diagnostic, DiagnosticSeverity};

use std::fs::{File, read_to_string};
use std::io::BufWriter;

pub use std::io::Write;

pub struct IoError {
    pub message: String,
    pub path: Option<String>,
}

impl Into<Diagnostic> for IoError {
    fn into(self) -> Diagnostic {
        Diagnostic {
            severity: DiagnosticSeverity::Error,
            file: self.path,
            module: None,
            loc: None,
            message: self.message,
        }
    }
}

pub fn read_file(file_name: &str) -> Result<String, IoError> {
    read_to_string(file_name).map_err(|error| IoError {
        message: error.to_string(),
        path: Some(file_name.to_owned()),
    })
}

pub fn create_file_writer(
    file_name: &str,
) -> Result<BufWriter<File>, IoError> {
    Ok(BufWriter::new(File::create(file_name).map_err(|error| {
        IoError {
            message: error.to_string(),
            path: Some(file_name.to_owned()),
        }
    })?))
}
