use crate::prelude::{LibType, ProjectConfig};
use boron_target::target::Target;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct BLibMetadata {
  pub config: ProjectConfig,
  pub target: Target,
  #[serde(default = "default_metadata_version")]
  pub metadata_version: u16,
  #[serde(default)]
  pub thir_debug: Option<String>,
  #[serde(default)]
  pub package: BLibPackageIdentity,
  #[serde(default)]
  pub abi_fingerprint: String,
  #[serde(default)]
  pub modules: Vec<BLibModule>,
  #[serde(default)]
  pub exports: Vec<BLibExport>,
  #[serde(default)]
  pub types: Vec<BLibType>,
  #[serde(default)]
  pub signatures: Vec<BLibSignature>,
  #[serde(default)]
  pub link: BLibLinkInfo,
  #[serde(default)]
  pub dependencies: Vec<BLibDependencyRef>,
}

fn default_metadata_version() -> u16 {
  1
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibPackageIdentity {
  pub name: String,
  pub version: Option<String>,
  pub source_hash: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibModule {
  pub id: u32,
  pub path: Vec<String>,
  pub file_hint: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default)]
pub enum BLibNamespace {
  #[default]
  Value,
  Type,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default)]
pub enum BLibExportKind {
  Function,
  Struct,
  Enum,
  Const,
  Static,
  Module,
  Trait,
  TypeAlias,
  #[default]
  Unknown,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default)]
pub enum BLibVisibility {
  #[default]
  Public,
  Private,
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibExport {
  pub name: String,
  pub namespace: BLibNamespace,
  pub kind: BLibExportKind,
  pub module_id: u32,
  pub visibility: BLibVisibility,
  pub type_id: Option<u32>,
  pub signature_id: Option<u32>,
  pub doc: Option<String>,
  pub deprecated: Option<String>,
  pub attrs: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default)]
pub enum BLibTypeKind {
  Primitive,
  Named,
  Pointer,
  Reference,
  Slice,
  Array,
  Tuple,
  Function,
  GenericParam,
  #[default]
  Unknown,
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibType {
  pub id: u32,
  pub kind: BLibTypeKind,
  pub name: Option<String>,
  pub args: Vec<u32>,
  pub is_mutable: bool,
  pub array_len: Option<u64>,
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct BLibSignature {
  pub id: u32,
  pub params: Vec<BLibParam>,
  pub return_type: Option<u32>,
  pub generics: Vec<String>,
  pub where_clauses: Vec<String>,
  pub calling_convention: Option<String>,
  pub flags: BLibSignatureFlags,
  pub abi_hash: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibParam {
  pub name: Option<String>,
  pub type_id: u32,
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibSignatureFlags {
  pub is_extern: bool,
  pub is_variadic: bool,
  pub is_const: bool,
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibLinkInfo {
  pub library_kind: LibType,
  pub artifact_name: String,
  pub search_paths: Vec<String>,
  pub libraries: Vec<String>,
  pub linker_flags: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq)]
pub struct BLibDependencyRef {
  pub name: String,
  pub version_req: Option<String>,
  pub abi_fingerprint: Option<String>,
  #[serde(default = "default_required_dependency")]
  pub required: bool,
}

fn default_required_dependency() -> bool {
  true
}

impl BLibMetadata {
  pub fn from_core(config: ProjectConfig, target: Target) -> Self {
    Self {
      config,
      target,
      metadata_version: default_metadata_version(),
      thir_debug: None,
      package: BLibPackageIdentity::default(),
      abi_fingerprint: String::new(),
      modules: Vec::new(),
      exports: Vec::new(),
      types: Vec::new(),
      signatures: Vec::new(),
      link: BLibLinkInfo::default(),
      dependencies: Vec::new(),
    }
  }
}
