use crate::prelude::{info, CompilationUnit, FILE_EXTENSION};
use anyhow::bail;
use anyhow::Result;
use boron_diagnostics::DiagnosticWriter;
use boron_session::dependency::Dependency;
use boron_session::prelude::{ProjectConfig, Session};
use yansi::Paint as _;

pub fn compiler_entrypoint(session: &Session) -> Result<()> {
  let packages = session.sorted_packages()?;

  for package in packages {
    let config = dependency_config(session, package)?;
    let pkg_session =
      Session::new(config, DiagnosticWriter::stderr(), session.compilation_mode());

    compile_single(&pkg_session)?;
  }

  compile_single(session)?;

  Ok(())
}

fn dependency_config(session: &Session, package: Dependency) -> Result<ProjectConfig> {
  let main_cfg = session.config();

  if let (Some(package_type), Some(lib_type), Some(diagnostic_output_type)) =
    (package.package_type, package.lib_type, package.diagnostic_output_type.clone())
  {
    return Ok(ProjectConfig {
      entrypoint: package.entrypoint,
      package_type,
      packages: vec![],
      mode: main_cfg.mode,
      name: package.name,
      lib_type,
      output: main_cfg.output.clone(),
      root: package.root,
      compiler: package.compiler.or(main_cfg.compiler),
      diagnostic_output_type,
      color: main_cfg.color,
      check_only: main_cfg.check_only,
      verbose: main_cfg.verbose,
      no_backtrace: main_cfg.no_backtrace,
      timings: false,
    });
  } else {
    bail!("couldn't get config for dependency: {}", package.name)
  }

  unreachable!()
}

fn compile_single(session: &Session) -> Result<()> {
  let file = &session.config.entrypoint;
  if let Some(ext) = file.extension() {
    if ext != FILE_EXTENSION {
      bail!(
        "Found an entry point with invalid extension: {}. It must be {}",
        file.display(),
        FILE_EXTENSION
      );
    }
  }

  let file = session.root().join(file);
  let contents = fs_err::read_to_string(file.clone())?;

  let file_id = session.sources().add(contents, file.clone());
  let mut unit = CompilationUnit::new(file_id, session);

  if session.config().check_only {
    unit.check();
    info!("{} without any problems", "Checked".underline().bold());
  } else {
    unit.build()?;
  }

  Ok(())
}
