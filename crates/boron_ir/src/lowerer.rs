use crate::{Ir, IrId, IrStruct, SymbolMangler};
use boron_analysis::ty::SubstitutionMap;
use boron_analysis::{InferTy, TypeScheme, TypeTable};
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
      self.lower_function(func.value());
    }

    for strukt in &self.hir.structs {
      self.lower_struct(strukt.value())
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

        let fields = self.lower_struct_fields(strukt, &mono.type_args, &scheme);
        let type_args: Vec<SemanticTy> = scheme
          .vars
          .iter()
          .filter_map(|param| {
            mono.type_args.get(param.def_id).map(|ty| self.lower_type(ty))
          })
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
      let fields = self.lower_struct_fields(strukt, &SubstitutionMap::new(), &scheme);
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
    scheme: &TypeScheme,
  ) -> Vec<(String, SemanticTy)> {
    strukt
      .fields
      .iter()
      .map(|f| {
        let original_ty =
          self.type_table.field_type(strukt.def_id, &f.name.text()).unwrap();
        let substituted_ty = self.apply_subst_by_def_id(&original_ty, type_args, scheme);
        (f.name.text(), self.lower_type(&substituted_ty))
      })
      .collect()
  }

  fn lower_type(&self, ty: &InferTy) -> SemanticTy {
    match ty {
      InferTy::Primitive(kind, _) => SemanticTy::Primitive(*kind),

      InferTy::Adt { def_id, args, .. } => {
        let fields = args.iter().map(|arg| self.lower_type(arg)).collect();
        SemanticTy::Struct { def_id: *def_id, fields }
      }

      InferTy::Ptr { mutability, ty: inner, .. } => SemanticTy::Ptr {
        mutability: *mutability,
        inner: Box::new(self.lower_type(inner)),
      },

      InferTy::Optional(inner, _) => {
        SemanticTy::Optional(Box::new(self.lower_type(inner)))
      }

      InferTy::Array { ty: inner, len, .. } => {
        SemanticTy::Array { elem: Box::new(self.lower_type(inner)), len: *len }
      }

      InferTy::Slice(inner, _) => SemanticTy::Slice(Box::new(self.lower_type(inner))),

      InferTy::Tuple(tys, _) => {
        SemanticTy::Tuple(tys.iter().map(|t| self.lower_type(t)).collect())
      }

      InferTy::Fn { params, ret, .. } => SemanticTy::Fn {
        params: params.iter().map(|p| self.lower_type(p)).collect(),
        ret: Box::new(self.lower_type(ret)),
      },

      InferTy::Unit(_) => SemanticTy::Unit,
      InferTy::Never(_) => SemanticTy::Never,

      InferTy::Var(_, _) | InferTy::Param(_) => {
        panic!("Cannot lower unresolved type variable or param to SemanticTy")
      }

      InferTy::Err(_) => SemanticTy::Error,
    }
  }

  fn apply_subst_by_def_id(
    &self,
    ty: &InferTy,
    type_args: &SubstitutionMap,
    scheme: &TypeScheme,
  ) -> InferTy {
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
        args: args
          .iter()
          .map(|t| self.apply_subst_by_def_id(t, type_args, scheme))
          .collect(),
        span: *span,
      },
      InferTy::Ptr { mutability, ty: inner, span } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(self.apply_subst_by_def_id(inner, type_args, scheme)),
        span: *span,
      },
      InferTy::Optional(inner, span) => InferTy::Optional(
        Box::new(self.apply_subst_by_def_id(inner, type_args, scheme)),
        *span,
      ),
      InferTy::Array { ty: inner, len, span } => InferTy::Array {
        ty: Box::new(self.apply_subst_by_def_id(inner, type_args, scheme)),
        len: *len,
        span: *span,
      },
      InferTy::Slice(inner, span) => InferTy::Slice(
        Box::new(self.apply_subst_by_def_id(inner, type_args, scheme)),
        *span,
      ),
      InferTy::Tuple(tys, span) => InferTy::Tuple(
        tys.iter().map(|t| self.apply_subst_by_def_id(t, type_args, scheme)).collect(),
        *span,
      ),
      InferTy::Fn { params, ret, span } => InferTy::Fn {
        params: params
          .iter()
          .map(|t| self.apply_subst_by_def_id(t, type_args, scheme))
          .collect(),
        ret: Box::new(self.apply_subst_by_def_id(ret, type_args, scheme)),
        span: *span,
      },
      _ => ty.clone(),
    }
  }

  pub fn lower_function(&self, _func: &Function) {}
}
