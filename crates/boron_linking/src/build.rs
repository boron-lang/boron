use crate::{
  detect::{detect_linker, resolve_from_kind},
  linker::{Linker, LinkerKind},
  linkers::{LinkerArgStyle, linker_tool},
};
use anyhow::{Context as _, Result, bail, ensure};
use boron_session::prelude::{LibType, PackageType, Session};
use log::{debug, info};
use std::{
  collections::HashSet,
  ffi::OsStr,
  path::{Path, PathBuf},
  process::Command,
  sync::Arc,
};
use yansi::Paint as _;

pub struct LinkerBuild<'a> {
  sess: &'a Session,
  linker: Linker,
  input_files: HashSet<Arc<OsStr>>,
  linker_flags: Vec<Arc<OsStr>>,
  library_dirs: HashSet<Arc<Path>>,
  libraries: HashSet<Arc<OsStr>>,
}

impl<'a> LinkerBuild<'a> {
  pub fn new(sess: &'a Session) -> Result<Self> {
    let _ = sess.create_output_dir();

    let linker = if let Some(kind) = sess.linker() {
      resolve_from_kind(LinkerKind::from(kind), sess)
        .with_context(|| format!("Specified linker not found: {kind:?}"))?
    } else {
      detect_linker(sess)?
    };

    Ok(Self {
      sess,
      linker,
      input_files: HashSet::new(),
      linker_flags: Vec::new(),
      library_dirs: HashSet::new(),
      libraries: HashSet::new(),
    })
  }

  pub fn linker_kind(&self) -> &LinkerKind {
    self.linker.kind()
  }

  pub fn sess(&self) -> &Session {
    self.sess
  }

  pub fn add_input<P: AsRef<OsStr>>(&mut self, path: P) -> &mut Self {
    self.input_files.insert(Arc::from(path.as_ref()));
    self
  }

  pub fn add_source<P: AsRef<OsStr>>(&mut self, path: P) -> &mut Self {
    self.add_input(path)
  }

  pub fn add_sources<I, P>(&mut self, paths: I) -> &mut Self
  where
    I: IntoIterator<Item = P>,
    P: AsRef<OsStr>,
  {
    for path in paths {
      self.add_input(path);
    }
    self
  }

  pub fn add_library_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
    self.library_dirs.insert(Arc::from(dir.as_ref()));
    self
  }

  pub fn add_library<S: AsRef<OsStr>>(&mut self, lib: S) -> &mut Self {
    self.libraries.insert(Arc::from(lib.as_ref()));
    self
  }

  pub fn add_linker_flag<S: AsRef<OsStr>>(&mut self, flag: S) -> &mut Self {
    self.linker_flags.push(Arc::from(flag.as_ref()));
    self
  }

  pub fn add_flag<S: AsRef<OsStr>>(&mut self, flag: S) -> &mut Self {
    self.add_linker_flag(flag)
  }

  pub fn link(&self, output_name: impl AsRef<OsStr>) -> Result<PathBuf> {
    ensure!(!self.input_files.is_empty(), "No input files specified");

    let output_path = match self.sess.config().project_type {
      PackageType::Library if self.sess.config().lib_type == LibType::Static => {
        self.create_static_library_from_inputs(output_name.as_ref())
      }
      _ => self.link_direct(output_name.as_ref()),
    }?;

    println!(
      "{} {}",
      "Finished".bold().underline(),
      format!("{}", output_path.display()).dim()
    );

    Ok(output_path)
  }

  fn create_static_library_from_inputs(&self, output_name: &OsStr) -> Result<PathBuf> {
    let input_files: Vec<Arc<OsStr>> = self.input_files.iter().cloned().collect();
    let output_path = self.create_static_library(output_name, &input_files)?;

    ensure!(
      output_path.exists(),
      "Library creation succeeded but output file was not created: {}",
      output_path.display()
    );

    Ok(output_path)
  }

  fn link_direct(&self, output_name: &OsStr) -> Result<PathBuf> {
    let mut command = Command::new(self.linker.path());
    let output_path = self.configure_link_command(&mut command, output_name)?;

    debug!("Executing linker command: {command:?}");

    info!("{} {}", "Linking with".underline(), self.linker.kind().name().bold());

    let output = command
      .current_dir(self.build_dir())
      .output()
      .context("Failed to execute linker")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    if !output.stdout.is_empty() {
      debug!("Linker stdout: {stdout}");
    }
    if !output.stderr.is_empty() {
      debug!("Linker stderr: {stderr}");
    }

    if !output.status.success() {
      if !stderr.is_empty() {
        bail!("Linking failed with error: {stderr}");
      } else if !stdout.is_empty() {
        bail!("Linking failed with error: {stdout}");
      } else {
        bail!("Linking failed with exit code: {}", output.status);
      }
    }

    ensure!(
      output_path.exists(),
      "Linking succeeded but output file was not created: {}",
      output_path.display()
    );

    Ok(output_path)
  }

  fn create_static_library(
    &self,
    output_name: &OsStr,
    object_files: &[Arc<OsStr>],
  ) -> Result<PathBuf> {
    let output_path = self.get_output_path(output_name);

    let target = self.sess.target();
    let mut command = match target.archiver() {
      Archiver::MsvcLib => {
        let mut cmd = Command::new(target.archiver_executable());
        cmd.arg(format!("/OUT:{}", output_path.display()));
        cmd
      }
      Archiver::LlvmAr => {
        let mut cmd = Command::new(target.archiver_executable());
        cmd.arg("rcs");
        cmd.arg(&output_path);
        cmd
      }
    };

    for obj_file in object_files {
      command.arg(obj_file);
    }

    debug!("Executing library creation command: {command:?}");

    let output = command
      .current_dir(self.build_dir())
      .output()
      .context("Failed to execute library archiver")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !output.stdout.is_empty() {
      debug!("Library creation stdout: {stdout}");
    }
    if !output.stderr.is_empty() {
      debug!("Library creation stderr: {stderr}");
    }

    if !output.status.success() {
      if !stderr.is_empty() {
        bail!("Library creation failed with error: {stderr}");
      } else if !stdout.is_empty() {
        bail!("Library creation failed with error: {stdout}");
      } else {
        bail!("Library creation failed with exit code: {}", output.status);
      }
    }

    Ok(output_path)
  }

  fn configure_link_command(
    &self,
    command: &mut Command,
    output_name: &OsStr,
  ) -> Result<PathBuf> {
    let style = self.linker_arg_style();

    for file in &self.input_files {
      command.arg(file);
    }

    for dir in &self.library_dirs {
      style.push_library_dir(command, dir.as_ref());
    }

    for lib in &self.libraries {
      style.push_library(command, lib.as_ref());
    }

    let output_path = self.configure_output(command, output_name, &style);

    for flag in &self.linker_flags {
      command.arg(flag);
    }

    Ok(output_path)
  }

  fn configure_output(
    &self,
    command: &mut Command,
    output_name: &OsStr,
    style: &LinkerArgStyle,
  ) -> PathBuf {
    match self.sess.config().project_type {
      PackageType::Library => match self.sess.config().lib_type {
        LibType::Static => {
          let output_path = self.get_output_path(output_name);
          style.push_output(command, &output_path);
          output_path
        }
        LibType::Dynamic => {
          let output_path = self.get_output_path(output_name);
          style.push_shared_library(command);
          style.push_output(command, &output_path);
          output_path
        }
      },
      PackageType::Binary => {
        let output_path = self.get_output_path(output_name);
        style.push_output(command, &output_path);
        output_path
      }
    }
  }

  pub fn disable_warnings(&mut self) -> &mut Self {
    let style = self.linker_arg_style();
    self.add_linker_flag(style.no_logo_flag);
    self
  }

  fn get_output_path(&self, output_name: &OsStr) -> PathBuf {
    let target = self.sess.target();
    let name = output_name.to_string_lossy();

    match self.sess.config().project_type {
      PackageType::Library => match self.sess.config().lib_type {
        LibType::Static => {
          let file_name = if name.starts_with(target.lib_prefix()) {
            format!("{name}{}", target.staticlib_suffix())
          } else {
            format!("{}{}{}", target.lib_prefix(), name, target.staticlib_suffix())
          };
          self.build_dir().join(file_name)
        }
        LibType::Dynamic => {
          let file_name = if name.starts_with(target.lib_prefix()) {
            format!("{name}{}", target.dll_suffix())
          } else {
            target.dll_name(&name)
          };
          self.build_dir().join(file_name)
        }
      },
      PackageType::Binary => {
        let mut file_name = name.to_string();
        file_name.push_str(target.exe_suffix());
        self.build_dir().join(file_name)
      }
    }
  }

  fn build_dir(&self) -> &Path {
    self.sess.config().output.as_path()
  }

  fn linker_arg_style(&self) -> LinkerArgStyle {
    linker_tool(self.linker.kind().clone()).arg_style(self.sess)
  }
}

use boron_target::target::{Archiver, Linker as TargetLinker};
impl From<TargetLinker> for LinkerKind {
  fn from(value: TargetLinker) -> Self {
    match value {
      TargetLinker::Ld => Self::Ld,
      TargetLinker::Lld => Self::Lld,
      TargetLinker::MsvcLink => Self::MsvcLink,
      TargetLinker::Mold => Self::Mold,
      TargetLinker::Gold => Self::Gold,
    }
  }
}
