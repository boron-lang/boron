use crate::library::BLibMetadata;
use anyhow::{Context as _, Result, ensure};
use boron_session::prelude::{ProjectConfig, Session};
use boron_target::target::Target;
use itertools::Itertools as _;
use postcard::{from_bytes, take_from_bytes, to_allocvec};
use serde::{Deserialize, Serialize};
use std::path::Path;

const MAGIC: [u8; 4] = *b"BOR0";
const VERSION: u16 = 1;

#[derive(Serialize, Deserialize)]
struct ContainerHeader {
  magic: [u8; 4],
  version: u16,
  blib_metadata: BLibMetadata,
  files: Vec<FileEntry>,
}

#[derive(Serialize, Deserialize, Clone)]
struct FileEntry {
  name: String,
  offset: u64,
  size: u64,
}

#[derive(Debug)]
pub struct ContainerFileEntry {
  pub name: String,
  pub data: Vec<u8>,
}

#[derive(Debug)]
pub struct BLibContainer {
  pub metadata: BLibMetadata,
  pub files: Vec<ContainerFileEntry>,
}

#[derive(Serialize)]
struct BLibMetadataRef<'a> {
  config: &'a ProjectConfig,
  target: &'a Target,
}

fn build_blib_metadata(sess: &Session) -> Result<BLibMetadata> {
  let metadata_ref = BLibMetadataRef { config: sess.config(), target: sess.target() };
  let bytes = to_allocvec(&metadata_ref)?;
  from_bytes(&bytes).map_err(Into::into)
}

pub fn write_container_file<P: AsRef<Path>>(path: P, sess: &Session) -> Result<()> {
  let mut offset = 0;
  let mut entries = vec![];

  for file in sess.archive_files.read().iter() {
    let data = fs_err::read(file)?;
    let entry =
      FileEntry { name: file.display().to_string(), offset, size: data.len() as u64 };

    entries.push((entry, data.clone()));
    offset += data.len() as u64;
  }

  let preliminary_header = ContainerHeader {
    magic: MAGIC,
    version: VERSION,
    blib_metadata: build_blib_metadata(sess)?,
    files: entries.iter().map(|(entry, _)| entry.clone()).collect_vec(),
  };
  let header_size = to_allocvec(&preliminary_header)?.len() as u64;

  for (entry, _) in &mut entries {
    entry.offset += header_size;
  }

  let final_header = ContainerHeader {
    magic: MAGIC,
    version: VERSION,
    blib_metadata: build_blib_metadata(sess)?,
    files: entries.iter().map(|(entry, _)| entry.clone()).collect_vec(),
  };
  let mut out = to_allocvec(&final_header)?;

  for (_, data) in &entries {
    out.extend(data);
  }

  fs_err::write(path, out).map_err(Into::into)
}

pub fn read_container_file<P: AsRef<Path>>(path: P) -> Result<BLibContainer> {
  let bytes = fs_err::read(path)?;
  let (header, _): (ContainerHeader, &[u8]) = take_from_bytes(&bytes)?;

  ensure!(header.magic == MAGIC, "Invalid .blib magic header");
  ensure!(header.version == VERSION, "Unsupported .blib version: {}", header.version);

  let mut files = Vec::with_capacity(header.files.len());
  for entry in header.files {
    let start = usize::try_from(entry.offset)
      .with_context(|| format!("Offset too large for entry `{}`", entry.name))?;
    let size = usize::try_from(entry.size)
      .with_context(|| format!("Size too large for entry `{}`", entry.name))?;
    let end = start
      .checked_add(size)
      .with_context(|| format!("Overflow while reading entry `{}`", entry.name))?;

    ensure!(end <= bytes.len(), "Entry `{}` points outside of .blib bounds", entry.name);

    files.push(ContainerFileEntry { name: entry.name, data: bytes[start..end].to_vec() });
  }

  Ok(BLibContainer { metadata: header.blib_metadata, files })
}
