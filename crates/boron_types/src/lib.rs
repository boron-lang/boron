use boron_source::new_id;

pub mod ast;
pub mod hir;
pub mod infer_ty;
pub mod literal_table;
pub mod resolver;
pub mod thir;
pub mod tokens;

new_id!(DepId);
