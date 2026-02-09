use crate::ty::ArrayLength;
use crate::{InferTy, TyChecker, TyVarKind};
use std::fmt::Write as _;

impl TyChecker<'_> {
  pub fn format_type(&self, ty: &InferTy) -> String {
    let mut out = String::new();
    if self._format_into(&mut out, ty).is_err() {
      "<couldn't format>".to_owned()
    } else {
      out
    }
  }

  fn _format_into(&self, f: &mut String, ty: &InferTy) -> Result<(), std::fmt::Error> {
    match &self.infcx.resolve(ty) {
      InferTy::Var(var, _) => match self.infcx.var_kinds.get(var).map(|k| *k.value()) {
        Some(TyVarKind::Integer) => write!(f, "<integer>")?,
        Some(TyVarKind::Float) => write!(f, "<float>")?,
        Some(TyVarKind::General) => {
          if let Some(subst) = self.infcx.substitution.get(var) {
            debug_assert!(
              !matches!(*subst, InferTy::Var(v, _) if v == *var),
              "self-referential substitution for {var}"
            );
            self._format_into(f, subst.value())?;
          } else {
            write!(f, "_")?;
          }
        }
        None => write!(f, "_")?,
      },

      InferTy::Param(param) => {
        write!(f, "{}", param.name)?;
      }

      InferTy::Primitive(kind, _) => {
        write!(f, "{kind}")?;
      }

      InferTy::Adt { def_id, args, .. } => {
        if let Some(def) = self.resolver.get_definition(*def_id) {
          write!(f, "{}", def.name)?;
        } else {
          write!(f, "{}", def_id.index())?;
        }

        if !args.is_empty() {
          write!(f, "<")?;
          for (i, arg) in args.iter().enumerate() {
            if i != 0 {
              write!(f, ", ")?;
            }
            self._format_into(f, arg)?;
          }
          write!(f, ">")?;
        }
      }

      InferTy::Ptr { mutability, ty, .. } => {
        write!(f, "*{mutability} ")?;
        self._format_into(f, ty)?;
      }

      InferTy::Optional(ty, _) => {
        write!(f, "?")?;
        self._format_into(f, ty)?;
      }

      InferTy::Array { ty, len, .. } => {
        write!(f, "[")?;
        self._format_into(f, ty)?;
        write!(
          f,
          "; {}]",
          match len {
            ArrayLength::Len(val) => val.to_string(),
            ArrayLength::Poisoned => "<error>".to_owned(),
          }
        )?;
      }

      InferTy::Slice(ty, _) => {
        write!(f, "[")?;
        self._format_into(f, ty)?;
        write!(f, "]")?;
      }

      InferTy::Tuple(tys, _) => {
        write!(f, "(")?;
        for (i, ty) in tys.iter().enumerate() {
          if i != 0 {
            write!(f, ", ")?;
          }
          self._format_into(f, ty)?;
        }
        if tys.len() == 1 {
          write!(f, ",")?;
        }
        write!(f, ")")?;
      }

      InferTy::Fn { params, ret, .. } => {
        write!(f, "fn(")?;
        for (i, p) in params.iter().enumerate() {
          if i != 0 {
            write!(f, ", ")?;
          }
          self._format_into(f, p)?;
        }
        write!(f, ") -> ")?;
        self._format_into(f, ret)?;
      }

      InferTy::Unit(_) => write!(f, "()")?,
      InferTy::Never(_) => write!(f, "!")?,
      InferTy::Err(_) => write!(f, "{{error}}")?,
    }

    Ok(())
  }
}
