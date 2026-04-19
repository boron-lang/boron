use crate::errors::BoronError;
use anyhow::Result;
use std::path::{Path, PathBuf};

/// Canonicalizes a path and strips the common prefix
pub fn canonicalize_with_strip<P: AsRef<Path>>(path: P) -> Result<PathBuf, BoronError> {
  let canonical = fs_err::canonicalize(path)?;
  Ok(strip_windows_long_path_prefix(canonical))
}

/// Creates file if it doesn't exist and then canonicalizes it.
pub fn canonicalize_or_create_dir<P: AsRef<Path>>(
  path: P,
) -> Result<PathBuf, BoronError> {
  fs_err::create_dir_all(path.as_ref())?;
  canonicalize_with_strip(path)
}

pub fn canonicalize_or_create_file<P: AsRef<Path>>(
  path: P,
) -> Result<PathBuf, BoronError> {
  let path = path.as_ref();
  if let Some(parent) = path.parent() {
    fs_err::create_dir_all(parent)?;
  }
  fs_err::OpenOptions::new().write(true).create(true).open(path)?;
  canonicalize_with_strip(path)
}

/// Strips the Windows long path prefix `\\?\` from a path if present.
///
/// On Windows, paths longer than 260 characters are prefixed with `\\?\` to bypass
/// the `MAX_PATH` limitation. This function removes that prefix to make paths more readable.
fn strip_windows_long_path_prefix(path: PathBuf) -> PathBuf {
  if cfg!(windows) {
    let path_str = path.to_string_lossy();

    if let Some(stripped) = path_str.strip_prefix(r"\\?\") {
      PathBuf::from(stripped)
    } else {
      path
    }
  } else {
    path
  }
}

/// Creates a directory and all its parent components if they are missing.
pub fn create_dir_all<P: AsRef<Path>>(path: P) -> Result<()> {
  let path = path.as_ref();
  if !path.exists() {
    fs_err::create_dir_all(path)?;
  }
  Ok(())
}
