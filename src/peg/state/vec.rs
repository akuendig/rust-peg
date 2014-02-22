use std::rc::{Rc};

use state::State;

#[deriving(Eq, Clone)]
pub struct VecState<A> {
  priv data: Rc<~[A]>,
  priv pos: uint,
  priv size: uint,
}

impl<A: Clone> VecState<A> {
  fn new_at(&self, pos: uint) -> VecState<A> {
    VecState{ data: self.data.clone(), pos: pos, size: self.size }
  }

  pub fn new(data: ~[A]) -> VecState<A> {
    let len = data.len();
    VecState{ data: Rc::new(data), pos: 0, size: len }
  }

  pub fn take(&self, n: uint) -> Option<(~[A], VecState<A>)> {
    if self.len() >= n {
      Some((self.data.borrow().slice(self.pos, self.pos + n).into_owned(), self.new_at(self.pos + n)))
    } else {
      None
    }
  }

  pub fn content(&self) -> ~[A] {
    self.data.borrow().clone()
  }
}

impl<A: Clone> State<A> for VecState<A> {
  fn head(&self) -> Option<(A, VecState<A>)> {
    if self.len() > 0 {
      Some((self.data.borrow()[self.pos].clone(), self.new_at(self.pos + 1)))
    } else {
      None
    }
  }

  fn skip(&self, n: uint) -> Option<VecState<A>> {
    if self.len() >= n {
      Some(self.new_at(self.pos + n))
    } else {
      None
    }
  }

  fn len(&self) -> uint {
    self.size - self.pos
  }
}

#[cfg(test)]
mod test_vec_state {
  use state::{State};
  use super::{VecState};

  #[test]
  fn head() {
    let values = ~[1, 2, 3];
    let (data, rest) = VecState::new(values).head().unwrap();

    assert_eq!(data, 1);
    assert_eq!(rest.len(), 2);

    let (data, rest) = rest.head().unwrap();

    assert_eq!(data, 2);

    let (data, rest) = rest.head().unwrap();

    assert_eq!(data, 3);
    assert_eq!(rest.head(), None);
  }

  #[test]
  fn head_twice() {
    let values = ~[1, 2, 3];
    let input = VecState::new(values);

    let (head1, _) = input.head().unwrap();
    let (head2, _) = input.head().unwrap();

    assert_eq!(head1, 1);
    assert_eq!(head2, 1);
  }

  #[test]
  fn take() {
    let values = ~[1, 2, 3];
    let (data, rest) = VecState::new(values).take(2).unwrap();

    assert_eq!(data, ~[1, 2]);
    assert_eq!(rest.len(), 1);

    let (data, rest) = rest.take(1).unwrap();

    assert_eq!(data, ~[3]);
    assert_eq!(rest.len(), 0);
    assert_eq!(rest.take(2), None);
  }

  #[test]
  fn skip(){
    let values = ~[1, 2, 3];
    let rest = VecState::new(values).skip(2).unwrap();

    assert_eq!(rest.len(), 1);

    let rest = rest.skip(1).unwrap();

    assert_eq!(rest.len(), 0);
    assert_eq!(rest.skip(2), None);
  }

  #[test]
  fn content() {
    let values = ~[1, 2, 3];
    assert_eq!(VecState::new(values).content(), ~[1, 2, 3]);

    let values: ~[int] = ~[];
    assert_eq!(VecState::new(values).content(), ~[]);
  }
}

