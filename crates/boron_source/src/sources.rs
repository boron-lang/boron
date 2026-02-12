use crate::source_file::{SourceFile, SourceFileId};
use dashmap::DashMap;
use dashmap::iter::Iter;
use dashmap::mapref::one::Ref;
use std::path::{Path, PathBuf};

#[derive(Debug, Default)]
pub struct SourcesImpl {
  pub sources: DashMap<SourceFileId, SourceFile>,
  pub path_to_id: DashMap<PathBuf, SourceFileId>,
}

/// This struct handles all sources (files) used in the compilation process.
#[derive(Debug, Default)]
pub struct Sources {
  inner: SourcesImpl,
  root: Option<PathBuf>,
}

impl Sources {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn with_root(root: PathBuf) -> Self {
    Self { inner: Default::default(), root: Some(root) }
  }

  pub fn add(&self, input: String, path: PathBuf) -> SourceFileId {
    if let Some(existing_id) = self.get_by_path(&path) {
      return *existing_id.value();
    }

    let id = SourceFileId::new();
    self.inner.sources.insert(id, SourceFile::new(input, path.clone(), id));
    self.inner.path_to_id.insert(path, id);
    id
  }

  pub fn get_by_path(&self, path: &PathBuf) -> Option<Ref<'_, PathBuf, SourceFileId>> {
    self.inner.path_to_id.get(path)
  }

  pub fn get(&self, id: SourceFileId) -> Option<Ref<'_, SourceFileId, SourceFile>> {
    self.inner.sources.get(&id)
  }

  pub fn get_unchecked(&self, id: SourceFileId) -> Ref<'_, SourceFileId, SourceFile> {
    self.inner.sources.get(&id).expect("source file should exist")
  }

  pub fn all(&self) -> Iter<'_, SourceFileId, SourceFile> {
    self.inner.sources.iter()
  }

  pub fn display(&self, id: SourceFileId) -> Option<String> {
    self.get(id).map(|s| {
      let path = s.path();

      self.strip_root(path.as_path()).display().to_string()
    })
  }

  pub fn strip_root(&self, path: &Path) -> PathBuf {
    let Some(root) = &self.root else {
      return path.to_path_buf();
    };

    strip_same_root(path, root)
  }
}

/// strips the same root from a path using `reference_path` as the common base
pub fn strip_same_root(path: &Path, reference_path: &Path) -> PathBuf {
  let path_components: Vec<_> = path.components().collect();
  let reference_components: Vec<_> = reference_path.components().collect();

  let mut common_length = 0;

  for (path_comp, ref_comp) in path_components.iter().zip(reference_components.iter()) {
    if path_comp == ref_comp {
      common_length += 1;
    } else {
      break;
    }
  }

  let stripped_path: PathBuf = path_components.iter().skip(common_length).collect();

  stripped_path
}
