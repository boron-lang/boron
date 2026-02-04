use crate::primitive::PrimitiveKind;
use strum::EnumString;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
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
pub enum Linker {
  #[strum(serialize = "ld")]
  Ld,
  #[strum(serialize = "lld")]
  Lld,
  #[strum(serialize = "link")]
  MsvcLink,
  #[strum(serialize = "mold")]
  Mold,
  #[strum(serialize = "gold")]
  Gold,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, EnumString)]
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

#[derive(Copy, Clone, PartialEq, Eq, Debug, EnumString)]
pub enum CCompiler {
  #[strum(serialize = "gcc")]
  Gcc,
  #[strum(serialize = "clang")]
  Clang,
  #[strum(serialize = "msvc")]
  Msvc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DataLayout {
  pub pointer_size: usize,
  pub pointer_align: usize,

  pub i8_size: usize,
  pub i8_align: usize,

  pub i16_size: usize,
  pub i16_align: usize,

  pub i32_size: usize,
  pub i32_align: usize,

  pub i64_size: usize,
  pub i64_align: usize,

  pub i128_size: usize,
  pub i128_align: usize,

  pub f32_size: usize,
  pub f32_align: usize,

  pub f64_size: usize,
  pub f64_align: usize,

  pub bool_size: usize,
  pub bool_align: usize,

  pub char_size: usize,
  pub char_align: usize,

  pub endian: Endian,
}

impl DataLayout {
  /// 64-bit little-endian data layout (`x86_64`, aarch64)
  pub fn new_64bit_little_endian() -> Self {
    Self {
      pointer_size: 8,
      pointer_align: 8,

      i8_size: 1,
      i8_align: 1,

      i16_size: 2,
      i16_align: 2,

      i32_size: 4,
      i32_align: 4,

      i64_size: 8,
      i64_align: 8,

      // i128 alignment varies by platform. x86_64: 16 bytes, aarch64: 16 bytes, but Windows x64: 8 bytes
      i128_size: 16,
      i128_align: 16,

      f32_size: 4,
      f32_align: 4,

      f64_size: 8,
      f64_align: 8,

      bool_size: 1,
      bool_align: 1,

      char_size: 4,
      char_align: 4,

      endian: Endian::Little,
    }
  }

  /// 32-bit little-endian data layout (x86, arm)
  pub fn new_32bit_little_endian() -> Self {
    Self {
      pointer_size: 4,
      pointer_align: 4,

      i8_size: 1,
      i8_align: 1,

      i16_size: 2,
      i16_align: 2,

      i32_size: 4,
      i32_align: 4,

      i64_size: 8,
      // On 32-bit systems, i64 alignment is often 4 bytes (x86) or 8 bytes (ARM)
      i64_align: 4,

      i128_size: 16,
      // On 32-bit systems, i128 alignment is typically 4 or 8 bytes
      i128_align: 4,

      f32_size: 4,
      f32_align: 4,

      f64_size: 8,
      // On 32-bit x86, f64 alignment is 4 bytes, but on ARM it's 8 bytes
      f64_align: 4,

      bool_size: 1,
      bool_align: 1,

      char_size: 4,
      char_align: 4,

      endian: Endian::Little,
    }
  }

  /// 64-bit big-endian data layout
  pub fn new_64bit_big_endian() -> Self {
    let mut layout = Self::new_64bit_little_endian();
    layout.endian = Endian::Big;
    layout
  }

  /// 32-bit big-endian data layout
  pub fn new_32bit_big_endian() -> Self {
    let mut layout = Self::new_32bit_little_endian();
    layout.endian = Endian::Big;
    layout
  }

  /// Windows x64 data layout (has different i128 alignment)
  pub fn new_windows_x64() -> Self {
    Self {
      pointer_size: 8,
      pointer_align: 8,

      i8_size: 1,
      i8_align: 1,

      i16_size: 2,
      i16_align: 2,

      i32_size: 4,
      i32_align: 4,

      i64_size: 8,
      i64_align: 8,

      // Windows x64 aligns i128 to 8 bytes, not 16
      i128_size: 16,
      i128_align: 8,

      f32_size: 4,
      f32_align: 4,

      f64_size: 8,
      f64_align: 8,

      bool_size: 1,
      bool_align: 1,

      char_size: 4,
      char_align: 4,

      endian: Endian::Little,
    }
  }

  /// Windows x86 data layout
  pub fn new_windows_x86() -> Self {
    Self {
      pointer_size: 4,
      pointer_align: 4,

      i8_size: 1,
      i8_align: 1,

      i16_size: 2,
      i16_align: 2,

      i32_size: 4,
      i32_align: 4,

      i64_size: 8,
      i64_align: 8, // MSVC aligns i64 to 8 bytes even on 32-bit

      i128_size: 16,
      i128_align: 8,

      f32_size: 4,
      f32_align: 4,

      f64_size: 8,
      f64_align: 8, // MSVC aligns f64 to 8 bytes

      bool_size: 1,
      bool_align: 1,

      char_size: 4,
      char_align: 4,

      endian: Endian::Little,
    }
  }

  /// ARM32 data layout (different alignments from x86)
  pub fn new_arm32() -> Self {
    Self {
      pointer_size: 4,
      pointer_align: 4,

      i8_size: 1,
      i8_align: 1,

      i16_size: 2,
      i16_align: 2,

      i32_size: 4,
      i32_align: 4,

      i64_size: 8,
      i64_align: 8, // ARM aligns i64 to 8 bytes

      i128_size: 16,
      i128_align: 8,

      f32_size: 4,
      f32_align: 4,

      f64_size: 8,
      f64_align: 8, // ARM aligns f64 to 8 bytes

      bool_size: 1,
      bool_align: 1,

      char_size: 4,
      char_align: 4,

      endian: Endian::Little,
    }
  }
}

impl DataLayout {
  pub fn size_of(&self, primitive: PrimitiveKind) -> usize {
    match primitive {
      PrimitiveKind::I8 | PrimitiveKind::U8 => self.i8_size,
      PrimitiveKind::I16 | PrimitiveKind::U16 => self.i16_size,
      PrimitiveKind::I32 | PrimitiveKind::U32 => self.i32_size,
      PrimitiveKind::I64 | PrimitiveKind::U64 => self.i64_size,
      PrimitiveKind::I128 | PrimitiveKind::U128 => self.i128_size,
      PrimitiveKind::ISize | PrimitiveKind::USize => self.pointer_size,
      PrimitiveKind::F32 => self.f32_size,
      PrimitiveKind::F64 => self.f64_size,
      PrimitiveKind::Bool => self.bool_size,
      PrimitiveKind::Char => self.char_size,
    }
  }

  pub fn align_of(&self, primitive: PrimitiveKind) -> usize {
    match primitive {
      PrimitiveKind::I8 | PrimitiveKind::U8 => self.i8_align,
      PrimitiveKind::I16 | PrimitiveKind::U16 => self.i16_align,
      PrimitiveKind::I32 | PrimitiveKind::U32 => self.i32_align,
      PrimitiveKind::I64 | PrimitiveKind::U64 => self.i64_align,
      PrimitiveKind::I128 | PrimitiveKind::U128 => self.i128_align,
      PrimitiveKind::ISize | PrimitiveKind::USize => self.pointer_align,
      PrimitiveKind::F32 => self.f32_align,
      PrimitiveKind::F64 => self.f64_align,
      PrimitiveKind::Bool => self.bool_align,
      PrimitiveKind::Char => self.char_align,
    }
  }
}

#[derive(Debug)]
pub struct Target {
  pub arch: Arch,
  pub os: Os,
  pub endian: Endian,
  pub pointer_width: PointerWidth,
  pub linker: Linker,
  pub c_compiler: CCompiler,
  pub data_layout: DataLayout,
  pub triple: &'static str,
}

impl Target {
  fn triple_for(os: Os, arch: Arch) -> &'static str {
    match (os, arch) {
      (Os::Windows, Arch::X86_64) => "x86_64-pc-windows-msvc",
      (Os::Windows, Arch::X86) => "i686-pc-windows-msvc",
      (Os::Windows, Arch::AArch64) => "aarch64-pc-windows-msvc",
      (Os::Windows, Arch::Arm) => "armv7-pc-windows-msvc",
      (Os::Windows, Arch::RiscV64) => "riscv64-pc-windows-msvc",

      (Os::Linux, Arch::X86_64) => "x86_64-unknown-linux-gnu",
      (Os::Linux, Arch::X86) => "i686-unknown-linux-gnu",
      (Os::Linux, Arch::AArch64) => "aarch64-unknown-linux-gnu",
      (Os::Linux, Arch::Arm) => "armv7-unknown-linux-gnueabihf",
      (Os::Linux, Arch::RiscV64) => "riscv64-unknown-linux-gnu",

      (Os::MacOs, Arch::X86_64) => "x86_64-apple-darwin",
      (Os::MacOs, Arch::X86) => "i686-apple-darwin",
      (Os::MacOs, Arch::AArch64) => "aarch64-apple-darwin",
      (Os::MacOs, Arch::Arm) => "armv7-apple-darwin",
      (Os::MacOs, Arch::RiscV64) => "riscv64-apple-darwin",
    }
  }

  pub fn host() -> Self {
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

    #[cfg(target_endian = "little")]
    let endian = Endian::Little;
    #[cfg(target_endian = "big")]
    let endian = Endian::Big;

    #[cfg(target_pointer_width = "64")]
    let pointer_width = PointerWidth::Bits64;
    #[cfg(target_pointer_width = "32")]
    let pointer_width = PointerWidth::Bits32;

    let (linker, c_compiler) = match os {
      Os::Windows => (Linker::MsvcLink, CCompiler::Msvc),
      Os::Linux => (Linker::Ld, CCompiler::Gcc),
      Os::MacOs => (Linker::Ld, CCompiler::Clang),
    };

    let data_layout = match (os, arch, pointer_width) {
      // Windows x64
      (Os::Windows, Arch::X86_64, PointerWidth::Bits64) => DataLayout::new_windows_x64(),
      // Windows x86
      (Os::Windows, Arch::X86, PointerWidth::Bits32) => DataLayout::new_windows_x86(),
      // ARM 32-bit
      (_, Arch::Arm, PointerWidth::Bits32) => DataLayout::new_arm32(),
      // Generic 64-bit (Linux x64, macOS x64, macOS ARM64, RISC-V 64)
      (_, _, PointerWidth::Bits64) => {
        if endian == Endian::Big {
          DataLayout::new_64bit_big_endian()
        } else {
          DataLayout::new_64bit_little_endian()
        }
      }
      // Generic 32-bit
      (_, _, PointerWidth::Bits32) => {
        if endian == Endian::Big {
          DataLayout::new_32bit_big_endian()
        } else {
          DataLayout::new_32bit_little_endian()
        }
      }
    };

    let triple = Self::triple_for(os, arch);

    Self { arch, os, endian, pointer_width, linker, c_compiler, data_layout, triple }
  }

  pub fn new(
    arch: Arch,
    os: Os,
    pointer_width: PointerWidth,
    endian: Endian,
    c_compiler: CCompiler,
  ) -> Self {
    let linker = match os {
      Os::Windows => Linker::MsvcLink,
      Os::Linux | Os::MacOs => Linker::Ld,
    };

    let data_layout = match (os, arch, pointer_width, endian) {
      // Windows x64
      (Os::Windows, Arch::X86_64, PointerWidth::Bits64, Endian::Little) => {
        DataLayout::new_windows_x64()
      }
      // Windows x86
      (Os::Windows, Arch::X86, PointerWidth::Bits32, Endian::Little) => {
        DataLayout::new_windows_x86()
      }
      // ARM 32-bit (Linux/other)
      (_, Arch::Arm, PointerWidth::Bits32, Endian::Little) => DataLayout::new_arm32(),
      // Generic 64-bit little-endian (Linux x64, macOS x64, macOS ARM64)
      (_, _, PointerWidth::Bits64, Endian::Little) => {
        DataLayout::new_64bit_little_endian()
      }
      // Generic 64-bit big-endian
      (_, _, PointerWidth::Bits64, Endian::Big) => DataLayout::new_64bit_big_endian(),
      // Generic 32-bit little-endian (Linux x86)
      (_, _, PointerWidth::Bits32, Endian::Little) => {
        DataLayout::new_32bit_little_endian()
      }
      // Generic 32-bit big-endian
      (_, _, PointerWidth::Bits32, Endian::Big) => DataLayout::new_32bit_big_endian(),
    };

    let triple = Self::triple_for(os, arch);

    Self { arch, os, endian, pointer_width, linker, c_compiler, data_layout, triple }
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

  pub fn dll_name(&self, name: &str) -> String {
    format!("{}{}{}", self.lib_prefix(), name, self.dll_suffix())
  }

  pub fn staticlib_name(&self, name: &str) -> String {
    format!("{}{}{}", self.lib_prefix(), name, self.staticlib_suffix())
  }

  pub fn c_compiler_flags(&self) -> Vec<&'static str> {
    let mut flags = vec![];

    match self.c_compiler {
      CCompiler::Gcc | CCompiler::Clang => {
        flags.push("-std=c11");
        match self.pointer_width {
          PointerWidth::Bits64 => flags.push("-m64"),
          PointerWidth::Bits32 => flags.push("-m32"),
        }
      }
      CCompiler::Msvc => {}
    }

    flags
  }

  pub fn required_headers() -> Vec<&'static str> {
    vec!["stdint.h", "stdbool.h", "stddef.h"]
  }

  pub fn size_of(&self, primitive: PrimitiveKind) -> usize {
    self.data_layout.size_of(primitive)
  }

  pub fn align_of(&self, primitive: PrimitiveKind) -> usize {
    self.data_layout.align_of(primitive)
  }
}
