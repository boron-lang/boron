use crate::{IrBlock, IrId};
use boron_hir::{HirId, SemanticTy};
use boron_resolver::DefId;

#[derive(Debug, Clone)]
pub struct IrFunction {
  pub id: IrId,
  pub def_id: DefId,
  /// The concrete type args this monomorphization was instantiated with
  pub type_args: Vec<SemanticTy>,
  pub name: String,
  pub params: Vec<IrParam>,
  pub return_type: SemanticTy,
  pub body: Option<IrBody>,
}

#[derive(Debug, Clone)]
pub struct IrParam {
  pub def_id: DefId,
  pub name: String,
  pub ty: SemanticTy,
}

#[derive(Debug, Clone)]
pub struct IrBody {
  pub entry: HirId,
  pub blocks: Vec<IrBlock>,
}

#[derive(Debug, Clone)]
pub struct IrStruct {
  pub id: IrId,
  pub def_id: DefId,
  /// The concrete type args this monomorphization was instantiated with
  pub type_args: Vec<SemanticTy>,
  pub name: String,
  pub fields: Vec<(String, SemanticTy)>,
}
