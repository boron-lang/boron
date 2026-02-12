use crate::data_layout::DataLayout;
use crate::primitive::PrimitiveKind;
use inkwell::OptimizationLevel;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, TargetMachine};
use inkwell::targets::{Target as LLVMTarget, TargetTriple};
use strum::{Display, EnumString};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Endian {
  Little,
  Big,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, EnumString)]
pub enum Os {
  #[strum(serialize = "windows")]
  Windows,
  #[strum(serialize = "linux")]
  Linux,
  #[strum(serialize = "macos")]
  #[expect(clippy::enum_variant_names)]
  MacOs,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, EnumString)]
pub enum Compiler {
  #[strum(serialize = "clang")]
  Clang,
  #[strum(serialize = "gcc")]
  Gcc,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, EnumString)]
pub enum Archiver {
  #[strum(serialize = "llvm-ar")]
  LlvmAr,
  #[strum(serialize = "lib")]
  MsvcLib,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, EnumString, Display)]
pub enum Arch {
  #[strum(serialize = "x86_64")]
  X86_64,
  #[strum(serialize = "x86")]
  X86,
  #[strum(serialize = "aarch64")]
  AArch64,
  #[strum(serialize = "arm")]
  Arm,
  #[strum(serialize = "riscv64")]
  RiscV64,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, EnumString)]
pub enum PointerWidth {
  #[strum(serialize = "32")]
  Bits32,
  #[strum(serialize = "64")]
  Bits64,
}

impl PointerWidth {
  pub fn size_bytes(&self) -> usize {
    match self {
      Self::Bits64 => 8,
      Self::Bits32 => 4,
    }
  }
}

#[derive(Debug)]
pub struct Target {
  pub arch: Arch,
  pub os: Os,
  pub pointer_width: PointerWidth,
  pub compiler: Compiler,
  pub archiver: Archiver,
  pub data_layout: DataLayout,
  pub triple: TargetTriple,
  pub target_machine: TargetMachine,
  pub is_msvc: bool,
}

impl Target {
  pub fn host() -> Self {
    LLVMTarget::initialize_native(&InitializationConfig::default())
      .expect("Failed to initialize native target");

    #[cfg(target_arch = "x86_64")]
    let arch = Arch::X86_64;
    #[cfg(target_arch = "x86")]
    let arch = Arch::X86;
    #[cfg(target_arch = "aarch64")]
    let arch = Arch::AArch64;
    #[cfg(target_arch = "arm")]
    let arch = Arch::Arm;
    #[cfg(target_arch = "riscv64")]
    let arch = Arch::RiscV64;

    #[cfg(target_os = "windows")]
    let os = Os::Windows;
    #[cfg(target_os = "linux")]
    let os = Os::Linux;
    #[cfg(target_os = "macos")]
    let os = Os::MacOs;

    #[cfg(target_pointer_width = "64")]
    let pointer_width = PointerWidth::Bits64;
    #[cfg(target_pointer_width = "32")]
    let pointer_width = PointerWidth::Bits32;

    let triple = TargetMachine::get_default_triple();

    let target =
      LLVMTarget::from_triple(&triple).expect("Could not get target from triple");
    let target_machine = target
      .create_target_machine(
        &triple,
        "generic",
        "",
        OptimizationLevel::Default,
        RelocMode::Default,
        CodeModel::Default,
      )
      .expect("Could not create target machine");

    let triple_str = triple.as_str().to_string_lossy().to_ascii_lowercase();

    let data_layout = DataLayout::create_from_llvm(&target_machine.get_target_data());
    let compiler = Self::default_compiler_for_triple(&triple_str);
    let archiver = Self::default_archiver_for_triple(&triple);
    let is_msvc = triple_str.contains("msvc");

    Self {
      arch,
      os,
      pointer_width,
      compiler,
      archiver,
      data_layout,
      triple,
      target_machine,
      is_msvc,
    }
  }

  pub fn exe_suffix(&self) -> &'static str {
    match self.os {
      Os::Windows => ".exe",
      Os::Linux | Os::MacOs => "",
    }
  }

  pub fn dll_suffix(&self) -> &'static str {
    match self.os {
      Os::Windows => ".dll",
      Os::Linux => ".so",
      Os::MacOs => ".dylib",
    }
  }

  pub fn staticlib_suffix(&self) -> &'static str {
    match self.os {
      Os::Windows => ".lib",
      Os::Linux | Os::MacOs => ".a",
    }
  }

  pub fn lib_prefix(&self) -> &'static str {
    match self.os {
      Os::Windows => "",
      Os::Linux | Os::MacOs => "lib",
    }
  }

  pub fn obj_file_suffix(&self) -> &'static str {
    match self.os {
      Os::Windows => ".obj",
      Os::Linux | Os::MacOs => ".o",
    }
  }

  pub fn dll_name(&self, name: &str) -> String {
    format!("{}{}{}", self.lib_prefix(), name, self.dll_suffix())
  }

  pub fn staticlib_name(&self, name: &str) -> String {
    format!("{}{}{}", self.lib_prefix(), name, self.staticlib_suffix())
  }

  pub fn size_of(&self, primitive: PrimitiveKind) -> usize {
    self.data_layout.size_of(primitive)
  }

  pub fn align_of(&self, primitive: PrimitiveKind) -> usize {
    self.data_layout.align_of(primitive)
  }

  pub fn triple(&self) -> String {
    self.triple.as_str().to_string_lossy().to_string()
  }

  pub fn archiver(&self) -> Archiver {
    self.archiver
  }

  pub fn archiver_executable(&self) -> &'static str {
    match self.archiver {
      Archiver::LlvmAr => "llvm-ar",
      Archiver::MsvcLib => "lib",
    }
  }

  fn default_compiler_for_triple(triple_str: &str) -> Compiler {
    let is_windows = triple_str.contains("windows");
    let is_msvc = triple_str.contains("msvc");
    let is_apple = triple_str.contains("apple")
      || triple_str.contains("darwin")
      || triple_str.contains("macos")
      || triple_str.contains("ios");

    if is_apple {
      return Compiler::Clang;
    }
    if is_windows && !is_msvc {
      return Compiler::Gcc;
    }

    if is_windows {
      return Compiler::Clang;
    }
    Compiler::Gcc
  }

  fn default_archiver_for_triple(triple: &TargetTriple) -> Archiver {
    let triple_str = triple.as_str().to_string_lossy().to_ascii_lowercase();
    let is_windows = triple_str.contains("windows");
    let is_msvc = triple_str.contains("msvc");

    if is_windows && is_msvc { Archiver::MsvcLib } else { Archiver::LlvmAr }
  }
}
