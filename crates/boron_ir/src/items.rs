use crate::IrId;
use boron_hir::SemanticTy;

#[derive(Debug, Clone)]
pub struct IrFunction {
  pub id: IrId,
  /// The concrete type args this monomorphization was instantiated with
  pub type_args: Vec<SemanticTy>,
  pub name: String,
  pub params: Vec<(String, SemanticTy)>,
}

#[derive(Debug, Clone)]
pub struct IrStruct {
  pub id: IrId,
  /// The concrete type args this monomorphization was instantiated with
  pub type_args: Vec<SemanticTy>,
  pub name: String,
  pub fields: Vec<(String, SemanticTy)>,
}
