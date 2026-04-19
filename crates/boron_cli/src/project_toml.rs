use crate::cli::CliBuild;
use anyhow::{Context as _, Result, anyhow, bail};
use boron_diagnostics::prelude::DiagnosticOutputType;
use boron_lib::container::read_container_file;
use boron_session::dependency::{DepId, Dependency};
use boron_session::enums::lib_type::LibType;
use boron_session::enums::mode::Mode;
use boron_session::enums::project_type::PackageType;
use boron_session::prelude::{
  BoronError, canonicalize_or_create_dir, canonicalize_with_strip,
};
use boron_session::project_config::ProjectConfig;
use boron_target::target::Compiler;
use fs_err as fs;
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
  if !path.exists() {
    return Ok(None);
  }

  let content = fs::read_to_string(path)?;
  let toml = toml::from_str(&content)
    .with_context(|| format!("failed to parse {}", path.display()))?;

  Ok(Some(toml))
}

pub fn build_project_config(
  cli: CliBuild,
) -> Result<(ProjectConfig, HashMap<String, TomlPackage>)> {
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

    canonicalize_or_create_dir(if path.is_absolute() { path } else { root.join(path) })?
  };

  let package_type =
    cli.ty.map(Into::into).or(toml.project.ty).unwrap_or(PackageType::Binary);
  let mode = cli.mode.map(Into::into).or(toml.build.mode).unwrap_or(Mode::Debug);
  let lib_type = toml.build.lib_type.unwrap_or(LibType::Static);

  let diagnostic_output_type =
    toml.build.diagnostic_output.unwrap_or(DiagnosticOutputType::HumanReadable);

  let mut packages: Vec<Dependency> = vec![];
  for (alias, pkg) in &toml.dependencies {
    if packages.iter().any(|p| &p.name == alias) {
      continue;
    }

    let dependency = resolve_dependency(&output, &mode, &root, &alias, pkg, false)?;
    packages.push(dependency);
  }

  Ok((
    ProjectConfig {
      entrypoint: canonicalize_with_strip(entrypoint)?,
      package_type,
      packages,
      mode,
      name,
      lib_type,
      output,
      root: canonicalize_with_strip(root)?,
      compiler: toml.build.compiler,
      diagnostic_output_type,
      color: !cli.no_color,
      check_only: cli.check_only,
      verbose: cli.verbose,
      no_backtrace: cli.no_backtrace,
      timings: cli.timings,
    },
    toml.dependencies,
  ))
}

pub fn resolve_dependency(
  main_output_dir: &Path,
  main_mode: &Mode,
  root: &Path,
  alias: &str,
  pkg: &TomlPackage,
  fetch_blib: bool,
) -> Result<Dependency> {
  let dep_root = resolve_path(root, &pkg.path)
    .with_context(|| format!("failed to resolve path for dependency `{alias}`"))?;

  let dep_project = load_dep_project(&dep_root, alias)?;
  let build = dep_project.build;
  let project = dep_project.project;

  let name = pkg.name.clone().or(project.name).unwrap_or_else(|| alias.to_string());
  let entrypoint = resolve_entrypoint(
    &dep_root,
    alias,
    pkg.entrypoint.as_ref().or(project.entrypoint.as_ref()),
  )?;
  let output = resolve_output(&dep_root, alias, build.output)?;

  let blib = if fetch_blib {
    let blib_path =
      main_output_dir.join(main_mode.to_string()).join(alias).with_extension("blib");

    if !blib_path.exists() {
      bail!("couldn't find .blib file for {alias}: {}", blib_path.display())
    }
    let blib = read_container_file(&blib_path)?;
    Some(blib.metadata)
  } else {
    None
  };

  Ok(Dependency {
    id: DepId::new(),
    name,
    root: dep_root,
    entrypoint,
    depends_on: dep_project.dependencies.into_keys().collect(),
    package_type: Some(project.ty.unwrap_or(PackageType::Binary)),
    output: Some(output),
    lib_type: Some(build.lib_type.unwrap_or(LibType::Static)),
    compiler: build.compiler,
    diagnostic_output_type: Some(
      build.diagnostic_output.unwrap_or(DiagnosticOutputType::HumanReadable),
    ),
    blib,
  })
}

fn resolve_path(root: &Path, path: &PathBuf) -> Result<PathBuf, BoronError> {
  let full = if path.is_absolute() { path } else { &root.join(path) };
  canonicalize_with_strip(full)
}

fn load_dep_project(dep_root: &Path, alias: &str) -> Result<ProjectToml> {
  let path = dep_root.join(PROJECT_FILE);
  load_project_toml(&path)
    .with_context(|| {
      format!(
        "failed to load `{PROJECT_FILE}` for dependency `{alias}` from {}",
        path.display()
      )
    })?
    .ok_or_else(|| {
      anyhow!("dependency `{alias}` is missing `{PROJECT_FILE}` at {}", path.display())
    })
}

fn resolve_entrypoint(
  dep_root: &Path,
  alias: &str,
  raw: Option<&PathBuf>,
) -> Result<PathBuf> {
  let path = raw.ok_or_else(|| {
    anyhow!("dependency `{alias}` is missing an `entrypoint` in `{PROJECT_FILE}`")
  })?;
  resolve_path(dep_root, path).with_context(|| {
    format!("failed to resolve entrypoint for dependency `{alias}` from `{PROJECT_FILE}`")
  })
}

fn resolve_output(dep_root: &Path, alias: &str, raw: Option<PathBuf>) -> Result<PathBuf> {
  let path = raw.ok_or_else(|| {
    anyhow!("dependency `{alias}` is missing `build.output` in `{PROJECT_FILE}`")
  })?;
  let full = if path.is_absolute() { path } else { dep_root.join(path) };
  canonicalize_or_create_dir(full).with_context(|| {
    format!(
      "failed to resolve output path for dependency `{alias}` from `{PROJECT_FILE}`"
    )
  })
}
