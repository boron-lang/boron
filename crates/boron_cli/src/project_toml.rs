use crate::cli::Cli;
use anyhow::{Context as _, Result, anyhow};
use boron_diagnostics::prelude::DiagnosticOutputType;
use boron_session::dependency::{DepId, Dependency};
use boron_session::enums::lib_type::LibType;
use boron_session::enums::mode::Mode;
use boron_session::enums::project_type::PackageType;
use boron_session::prelude::canonicalize_with_strip;
use boron_session::project_config::ProjectConfig;
use boron_target::target::Compiler;
use fs_err as fs;
use fs_err::create_dir_all;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub const PROJECT_FILE: &str = "project.toml";

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct TomlPackage {
  pub path: PathBuf,
  pub name: Option<String>,
  pub entrypoint: Option<PathBuf>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct TomlProject {
  pub name: Option<String>,
  pub entrypoint: Option<PathBuf>,
  #[serde(rename = "type")]
  pub ty: Option<PackageType>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct TomlBuild {
  pub mode: Option<Mode>,
  pub output: Option<PathBuf>,
  pub lib_type: Option<LibType>,
  pub compiler: Option<Compiler>,
  pub diagnostic_output: Option<DiagnosticOutputType>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ProjectToml {
  #[serde(default)]
  pub project: TomlProject,
  #[serde(default)]
  pub build: TomlBuild,
  #[serde(default)]
  pub dependencies: HashMap<String, TomlPackage>,
}

pub fn load_project_toml(path: &Path) -> Result<Option<ProjectToml>> {
  path;
  if !path.exists() {
    return Ok(None);
  }

  let content = fs::read_to_string(path)?;
  let toml = toml::from_str(&content)
    .with_context(|| format!("failed to parse {}", path.display()))?;

  Ok(Some(toml))
}

pub fn build_project_config(cli: Cli) -> Result<ProjectConfig> {
  let root = cli.project.parent().unwrap();
  let toml = load_project_toml(&cli.project)?.unwrap_or_default();

  let entrypoint = root.join(
    toml
      .project
      .entrypoint
      .ok_or_else(|| anyhow!("entrypoint is required in project.toml"))?,
  );
  let name = toml
    .project
    .name
    .ok_or_else(|| anyhow!("project name is required (provide via project.toml)"))?;

  let output = {
    let path = cli
      .output
      .or(toml.build.output)
      .ok_or_else(|| anyhow!("output path is required (provide via project.toml)"))?;

    if path.is_absolute() { path } else { root.join(path) }
  };

  let package_type =
    cli.ty.map(Into::into).or(toml.project.ty).unwrap_or(PackageType::Binary);
  let mode = cli.mode.map(Into::into).or(toml.build.mode).unwrap_or(Mode::Debug);
  let lib_type = toml.build.lib_type.unwrap_or(LibType::Static);

  let diagnostic_output_type =
    toml.build.diagnostic_output.unwrap_or(DiagnosticOutputType::HumanReadable);

  let mut packages: Vec<Dependency> = vec![];
  for (alias, pkg) in toml.dependencies {
    if packages.iter().any(|p| p.name == alias) {
      continue;
    }

    let dependency = resolve_dependency(root, &alias, pkg)?;
    packages.push(dependency);
  }

  Ok(ProjectConfig {
    entrypoint: canonicalize_with_strip(entrypoint)?,
    package_type,
    packages,
    mode,
    name,
    lib_type,
    output: canonicalize_with_strip(output)?,
    root: canonicalize_with_strip(root)?,
    compiler: toml.build.compiler,
    diagnostic_output_type,
    color: !cli.no_color,
    check_only: cli.check_only,
    verbose: cli.verbose,
    no_backtrace: cli.no_backtrace,
    timings: cli.timings,
  })
}

fn resolve_dependency(root: &Path, alias: &str, pkg: TomlPackage) -> Result<Dependency> {
  let dep_root = if pkg.path.is_absolute() { pkg.path } else { root.join(pkg.path) };

  let dep_root = canonicalize_with_strip(dep_root)
    .with_context(|| format!("failed to resolve path for dependency `{alias}`"))?;

  let dep_project_path = dep_root.join(PROJECT_FILE);
  let dep_project = load_project_toml(&dep_project_path)
    .with_context(|| {
      format!(
        "failed to load `{PROJECT_FILE}` for dependency `{alias}` from {}",
        dep_project_path.display()
      )
    })?
    .ok_or_else(|| {
      anyhow!(
        "dependency `{alias}` is missing `{PROJECT_FILE}` at {}",
        dep_project_path.display()
      )
    })?;

  let dep_name =
    pkg.name.or(dep_project.project.name).unwrap_or_else(|| alias.to_owned());

  let dep_entrypoint =
    pkg.entrypoint.or(dep_project.project.entrypoint).ok_or_else(|| {
      anyhow!("dependency `{alias}` is missing an `entrypoint` in `{PROJECT_FILE}`")
    })?;

  let dep_entrypoint = if dep_entrypoint.is_absolute() {
    dep_entrypoint
  } else {
    dep_root.join(dep_entrypoint)
  };

  let dep_entrypoint = canonicalize_with_strip(dep_entrypoint).with_context(|| {
    format!("failed to resolve entrypoint for dependency `{alias}` from `{PROJECT_FILE}`")
  })?;

  let dep_output = dep_project.build.output.ok_or_else(|| {
    anyhow!("dependency `{alias}` is missing `build.output` in `{PROJECT_FILE}`")
  })?;

  let dep_output =
    if dep_output.is_absolute() { dep_output } else { dep_root.join(dep_output) };

  create_dir_all(&dep_output)?;
  let dep_output = canonicalize_with_strip(dep_output).with_context(|| {
    format!(
      "failed to resolve output path for dependency `{alias}` from `{PROJECT_FILE}`"
    )
  })?;

  let package_type = dep_project.project.ty.unwrap_or(PackageType::Binary);
  let lib_type = dep_project.build.lib_type.unwrap_or(LibType::Static);
  let compiler = dep_project.build.compiler;
  let diagnostic_output_type =
    dep_project.build.diagnostic_output.unwrap_or(DiagnosticOutputType::HumanReadable);

  let depends_on = dep_project.dependencies.into_keys().collect();

  Ok(Dependency {
    id: DepId::new(),
    name: dep_name,
    root: dep_root,
    entrypoint: dep_entrypoint,
    depends_on,
    package_type: Some(package_type),
    output: Some(dep_output),
    lib_type: Some(lib_type),
    compiler,
    diagnostic_output_type: Some(diagnostic_output_type),
  })
}
