use crate::prelude::{CompilationUnit, FILE_EXTENSION, debug, info};
use anyhow::Result;
use anyhow::bail;
use boron_cli::prelude::{TomlPackage, resolve_dependency};
use boron_context::BCtx;
use boron_diagnostics::DiagnosticWriter;
use boron_session::dependency::Dependency;
use boron_session::prelude::{ProjectConfig, Session};
use std::collections::HashMap;
use yansi::Paint as _;

pub fn compiler_entrypoint(
  session: &mut Session,
  dependencies: HashMap<String, TomlPackage>,
) -> Result<()> {
  let ctx = BCtx::new();
  let package_deps = session.sorted_packages()?;
  let mut package_sessions = Vec::new();

  for package in package_deps {
    let config = dependency_config(session, package)?;
    debug!(name: "compiling package: ", config = ?config);
    let pkg_session =
      Session::new(config, DiagnosticWriter::stderr(), session.compilation_mode());
    package_sessions.push(pkg_session);
  }

  for pkg_session in &package_sessions {
    compile_single(pkg_session, &ctx)?;
  }

  let mut packages: Vec<Dependency> = vec![];
  for (alias, pkg) in dependencies {
    if packages.iter().any(|p| p.name == alias) {
      continue;
    }

    let dependency = resolve_dependency(
      &session.config.output,
      &session.config.mode,
      &session.config.root,
      &alias,
      &pkg,
      true,
    )?;
    packages.push(dependency);
  }

  session.config.packages = packages;

  compile_single(session, &ctx)?;

  Ok(())
}

fn dependency_config(session: &Session, package: &Dependency) -> Result<ProjectConfig> {
  let main_cfg = session.config();

  if let (Some(package_type), Some(lib_type), Some(diagnostic_output_type)) =
    (package.package_type, package.lib_type, package.diagnostic_output_type.clone())
  {
    return Ok(ProjectConfig {
      entrypoint: package.entrypoint.clone(),
      package_type,
      packages: vec![],
      mode: main_cfg.mode,
      name: package.name.clone(),
      version: package.version.clone(),
      lib_type,
      output: main_cfg.output.clone(),
      root: package.root.clone(),
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

fn compile_single<'ctx>(session: &'ctx Session, ctx: &'ctx BCtx<'ctx>) -> Result<()> {
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
  let mut unit = CompilationUnit::new(file_id, session, ctx);

  if session.config().check_only {
    unit.check();
    info!("{} without any problems", "Checked".underline().bold());
  } else {
    unit.build()?;
  }

  Ok(())
}
