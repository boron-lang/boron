use crate::errors::BoronError;
use anyhow::Result;
use std::path::{Path, PathBuf};

/// Canonicalizes a path and strips the common prefix
pub fn canonicalize_with_strip<P: AsRef<Path>>(path: P) -> Result<PathBuf, BoronError> {
  let canonical = fs_err::canonicalize(path)?;
  Ok(strip_windows_long_path_prefix(canonical))
}

/// Strips the Windows long path prefix `\\?\` from a path if present.
///
/// On Windows, paths longer than 260 characters are prefixed with `\\?\` to bypass
/// the `MAX_PATH` limitation. This function removes that prefix to make paths more readable.
fn strip_windows_long_path_prefix(path: PathBuf) -> PathBuf {
  #[cfg(windows)]
  {
    let path_str = path.to_string_lossy();

    if let Some(stripped) = path_str.strip_prefix(r"\\?\") {
      PathBuf::from(stripped)
    } else {
      path
    }
  }

  #[cfg(not(windows))]
  {
    // On non-Windows platforms, just return the path as-is
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
