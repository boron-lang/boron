use crate::IrBlock;
use crate::IrId;
use boron_hir::SemanticTy;
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
  pub block: IrBlock,
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

#[derive(Debug, Clone)]
pub struct IrEnumVariant {
  pub name: String,
  pub discriminant: u64,
  pub payload: Option<SemanticTy>,
}

#[derive(Debug, Clone)]
pub struct IrEnum {
  pub id: IrId,
  pub def_id: DefId,
  pub type_args: Vec<SemanticTy>,
  pub name: String,
  pub variants: Vec<IrEnumVariant>,
}
