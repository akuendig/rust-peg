pub use state::iter::IterState;
pub use state::vec::VecState;
pub use state::string::StrState;

pub mod iter;
pub mod vec;
pub mod string;

pub trait State<A>: Clone {
  fn head(&self) -> Option<(A, Self)>;
  fn skip(&self, n: uint) -> Option<Self>;
  fn len(&self) -> uint;
}

