use crate::{Ir, IrId, IrStruct, SymbolMangler};
use boron_analysis::ty::SubstitutionMap;
use boron_analysis::{InferTy, TypeTable};
use boron_hir::{Function, Hir, SemanticTy, Struct};

#[derive(Debug)]
pub struct IrLowerer<'a> {
  hir: &'a Hir,
  type_table: &'a TypeTable,
  ir: Ir,
  mangler: SymbolMangler<'a>,
}

impl<'a> IrLowerer<'a> {
  pub fn new(hir: &'a Hir, type_table: &'a TypeTable) -> Self {
    Self { hir, type_table, ir: Ir::default(), mangler: SymbolMangler::new(hir) }
  }

  pub fn mangler(&self) -> &SymbolMangler<'a> {
    &self.mangler
  }

  pub fn lower(&mut self) -> Ir {
    for func in &self.hir.functions {
      Self::lower_function(func.value());
    }

    for strukt in &self.hir.structs {
      self.lower_struct(strukt.value());
    }

    self.ir.clone()
  }

  pub fn lower_struct(&mut self, strukt: &Struct) {
    let scheme = self.type_table.def_type(strukt.def_id).unwrap();

    if let Some(monomorphizations) = self.type_table.monomorphizations.get(&strukt.def_id)
    {
      for mono in monomorphizations.iter() {
        if mono.type_args.map().iter().any(|(_, ty)| ty.has_params()) {
          continue;
        }

        let fields = self.lower_struct_fields(strukt, &mono.type_args);
        let type_args: Vec<SemanticTy> = scheme
          .vars
          .iter()
          .filter_map(|param| mono.type_args.get(param.def_id).map(Self::lower_type))
          .collect();

        let mangled_name = self.mangler.mangle_struct(strukt.def_id, &type_args);

        self.ir.structs.push(IrStruct {
          name: mangled_name,
          fields,
          id: IrId::new(),
          type_args,
        });
      }
    } else {
      let fields = self.lower_struct_fields(strukt, &SubstitutionMap::new());
      let mangled_name = self.mangler.mangle_struct(strukt.def_id, &[]);
      self.ir.structs.push(IrStruct {
        name: mangled_name,
        fields,
        id: IrId::new(),
        type_args: vec![],
      });
    }
  }

  fn lower_struct_fields(
    &self,
    strukt: &Struct,
    type_args: &SubstitutionMap,
  ) -> Vec<(String, SemanticTy)> {
    strukt
      .fields
      .iter()
      .map(|f| {
        let original_ty = self
          .type_table
          .field_type(strukt.def_id, &f.name.text())
          .expect("should be known");
        let substituted_ty = Self::apply_subst_by_def_id(&original_ty, type_args);
        (f.name.text(), Self::lower_type(&substituted_ty))
      })
      .collect()
  }

  fn lower_type(ty: &InferTy) -> SemanticTy {
    match ty {
      InferTy::Primitive(kind, _) => SemanticTy::Primitive(*kind),

      InferTy::Adt { def_id, args, .. } => {
        let fields = args.iter().map(Self::lower_type).collect();
        SemanticTy::Struct { def_id: *def_id, fields }
      }

      InferTy::Ptr { mutability, ty: inner, .. } => SemanticTy::Ptr {
        mutability: *mutability,
        inner: Box::new(Self::lower_type(inner)),
      },

      InferTy::Optional(inner, _) => {
        SemanticTy::Optional(Box::new(Self::lower_type(inner)))
      }

      InferTy::Array { ty: inner, len, .. } => {
        SemanticTy::Array { elem: Box::new(Self::lower_type(inner)), len: *len }
      }

      InferTy::Slice(inner, _) => SemanticTy::Slice(Box::new(Self::lower_type(inner))),

      InferTy::Tuple(tys, _) => {
        SemanticTy::Tuple(tys.iter().map(Self::lower_type).collect())
      }

      InferTy::Fn { params, ret, .. } => SemanticTy::Fn {
        params: params.iter().map(Self::lower_type).collect(),
        ret: Box::new(Self::lower_type(ret)),
      },

      InferTy::Unit(_) => SemanticTy::Unit,
      InferTy::Never(_) => SemanticTy::Never,

      InferTy::Var(_, _) | InferTy::Param(_) => {
        panic!("Cannot lower unresolved type variable or param to SemanticTy")
      }

      InferTy::Err(_) => SemanticTy::Error,
    }
  }

  fn apply_subst_by_def_id(ty: &InferTy, type_args: &SubstitutionMap) -> InferTy {
    match ty {
      InferTy::Var(_var, _span) => ty.clone(),
      InferTy::Param(param) => {
        if let Some(ty) = type_args.get(param.def_id) {
          ty.clone()
        } else {
          panic!()
        }
      }
      InferTy::Adt { def_id, args, span } => InferTy::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| Self::apply_subst_by_def_id(t, type_args)).collect(),
        span: *span,
      },
      InferTy::Ptr { mutability, ty: inner, span } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(Self::apply_subst_by_def_id(inner, type_args)),
        span: *span,
      },
      InferTy::Optional(inner, span) => {
        InferTy::Optional(Box::new(Self::apply_subst_by_def_id(inner, type_args)), *span)
      }
      InferTy::Array { ty: inner, len, span } => InferTy::Array {
        ty: Box::new(Self::apply_subst_by_def_id(inner, type_args)),
        len: *len,
        span: *span,
      },
      InferTy::Slice(inner, span) => {
        InferTy::Slice(Box::new(Self::apply_subst_by_def_id(inner, type_args)), *span)
      }
      InferTy::Tuple(tys, span) => InferTy::Tuple(
        tys.iter().map(|t| Self::apply_subst_by_def_id(t, type_args)).collect(),
        *span,
      ),
      InferTy::Fn { params, ret, span } => InferTy::Fn {
        params: params
          .iter()
          .map(|t| Self::apply_subst_by_def_id(t, type_args))
          .collect(),
        ret: Box::new(Self::apply_subst_by_def_id(ret, type_args)),
        span: *span,
      },
      _ => ty.clone(),
    }
  }

  pub fn lower_function(_func: &Function) {}
}
