#[macro_export]
macro_rules! queries {
  ($($tokens:tt)*) => {
    $crate::__queries_impl! {
      fields: [],
      impls: [],
      rest: [$($tokens)*],
    }
  };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __queries_impl {
  // base case
  (
    fields: [$($field:tt)*],
    impls:  [$($impl_:tt)*],
    rest:   [],
  ) => {
    paste::paste! {
      #[derive(Debug, Default)]
      pub struct Queries<'ctx> {
        $($field)*
      }
    }
    impl<'ctx> BCtx<'ctx> {
      $($impl_)*
    }
  };

  // #[no_cache]
  (
    fields: [$($field:tt)*],
    impls:  [$($impl_:tt)*],
    rest: [
      $(#[doc = $doc:expr])*
      #[no_cache]
      query $name:ident($($param_name:ident: $param_ty:ty),*): $ret_ty:ty;
      $($rest:tt)*
    ],
  ) => {
    $crate::__queries_impl! {
      fields: [
        $($field)*
        $(#[doc = $doc])*
        pub $name: Option<fn(&'ctx BCtx<'ctx>, ($($param_ty,)*)) -> $ret_ty>,
      ],
      impls: [
        $($impl_)*
        $(#[doc = $doc])*
        pub fn $name(&'ctx self, $($param_name: $param_ty),*) -> $ret_ty {
          (self.queries.$name
            .expect(concat!(
              "attempted to call ", stringify!($name),
              " query without its implementation",
            ))
          )(self, ($($param_name,)*))
        }
      ],
      rest: [$($rest)*],
    }
  };

  // cached
  (
    fields: [$($field:tt)*],
    impls:  [$($impl_:tt)*],
    rest: [
      $(#[doc = $doc:expr])*
      query $name:ident($($param_name:ident: $param_ty:ty),*): $ret_ty:ty;
      $($rest:tt)*
    ],
  ) => {
    $crate::__queries_impl! {
      fields: [
        $($field)*
        $(#[doc = $doc])*
        pub $name: Option<fn(&'ctx BCtx<'ctx>, ($($param_ty,)*)) -> $ret_ty>,
        pub [<$name _cache>]: std::cell::RefCell<
          std::collections::HashMap<($($param_ty,)*), $ret_ty>
        >,
      ],
      impls: [
        $($impl_)*
        $(#[doc = $doc])*
        pub fn $name(&'ctx self, $($param_name: $param_ty),*) -> $ret_ty {
          let params = ($($param_name,)*);
          paste::paste! {
            if let Some(cached) = self.queries.[<$name _cache>].borrow().get(&params) {
              return cached.clone();
            }
          }
          let f = self.queries.$name.expect(concat!(
            "attempted to call ", stringify!($name),
            " query without its implementation",
          ));
          let result = f(self, params.clone());
          paste::paste! {
            self.queries.[<$name _cache>].borrow_mut().insert(params, result.clone());
          }
          result
        }
      ],
      rest: [$($rest)*],
    }
  };
}
