use boron_source::new_id;

pub mod ast;
pub mod comptime;
pub mod hir;
pub mod infer_ty;
pub mod ir;
pub mod literal_table;
pub mod resolver;
pub mod thir;
pub mod tokens;
pub mod type_table;

new_id!(DepId);
