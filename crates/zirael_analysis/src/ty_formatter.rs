use crate::{InferTy, TyChecker, TyVarKind};
use std::fmt::Write;

impl TyChecker<'_> {
  pub fn format_type(&self, ty: &InferTy) -> String {
    self
      ._format(ty)
      .unwrap_or_else(|_| "<couldn't format>".to_string())
  }

  fn _format(&self, ty: &InferTy) -> Result<String, std::fmt::Error> {
    let mut f = String::new();
    match ty {
      InferTy::Var(var, _) => match self.infcx.var_kinds.get(var) {
        Some(kind) => write!(
          f,
          "{}",
          match kind.value() {
            TyVarKind::Integer => "integer",
            TyVarKind::Float => "float",
            TyVarKind::General => "type variable",
          }
        )?,
        None => write!(f, "{}", var)?,
      },
      InferTy::Primitive(kind, _) => write!(f, "{}", kind)?,
      InferTy::Adt { def_id, args, .. } => {
        let name = self.resolver.get_definition(*def_id);

        write!(
          f,
          "{}",
          if let Some(def) = name {
            def.name
          } else {
            def_id.0.to_string()
          }
        )?;
        if !args.is_empty() {
          write!(f, "<")?;
          for (i, arg) in args.iter().enumerate() {
            if i > 0 {
              write!(f, ", ")?;
            }
            write!(f, "{}", self.format_type(arg))?;
          }
          write!(f, ">")?;
        }
      }
      InferTy::Ptr { mutability, ty, .. } => {
        write!(f, "*{} {}", mutability, self.format_type(ty))?;
      }
      InferTy::Optional(ty, _) => write!(f, "?{}", self.format_type(ty))?,
      InferTy::Array { ty, len, .. } => {
        write!(f, "[{}; {}]", self.format_type(ty), len)?;
      }
      InferTy::Slice(ty, _) => write!(f, "[{}]", self.format_type(ty))?,
      InferTy::Tuple(tys, _) => {
        write!(f, "(")?;
        for (i, ty) in tys.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", self.format_type(ty))?;
        }
        if tys.len() == 1 {
          write!(f, ",")?;
        }
        write!(f, ")")?;
      }
      InferTy::Fn { params, ret, .. } => {
        write!(f, "fn(")?;
        for (i, param) in params.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", self.format_type(param))?;
        }
        write!(f, ") -> {}", self.format_type(ret))?;
      }
      InferTy::Unit(_) => write!(f, "()")?,
      InferTy::Never(_) => write!(f, "!")?,
      InferTy::Param { name, .. } => write!(f, "{}", name)?,
      InferTy::Err(_) => write!(f, "{{error}}")?,
    }

    Ok(f)
  }
}
