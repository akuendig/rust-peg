use std::str::{CharRange};
use std::rc::{Rc};

pub trait State<A>: Clone {
  fn head(&self) -> Option<(A, Self)>;
  fn skip(&self, n: uint) -> Option<Self>;
  fn len(&self) -> uint;
}

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

#[deriving(Eq, ToStr, Clone)]
pub struct IterState<T> {
    priv data: T,
}

impl<A: Clone, T: Clone + Iterator<A>> IterState<T> {
  pub fn new(data: T) -> IterState<T> {
    IterState{ data: data }
  }

  fn take(&self, n: uint) -> Option<(~[A], IterState<T>)> {
    let mut cpy = self.data.clone();
    let mut arr = ~[];
    let mut i = n;

    while i > 0 {
      match cpy.next() {
        Some(el) => arr.push(el),
        None => return None,
      }

      i -= 1
    }

    Some((arr, IterState::new(cpy)))
  }

  pub fn content(&self) -> T {
    self.data.clone()
  }
}

impl<A: Clone, T: Clone + Iterator<A>> State<A> for IterState<T> {
  fn head(&self) -> Option<(A, IterState<T>)> {
    let mut cpy = self.data.clone();

    let head = cpy.next();

    // We need to clone the cpy variable inside the closure,
    // because we are not allowed to move out of a captured
    // variable of a closure.
    head.map(|c: A| (c, IterState{ data: cpy.clone() }))
  }

  fn skip(&self, n: uint) -> Option<IterState<T>> {
    let mut cpy = self.data.clone();
    let mut i = n;

    while i > 0 {
      match cpy.next() {
        Some(..) => (),
        None => return None,
      }

      i -= 1;
    }

    Some(IterState{ data: cpy })
  }

  fn len(&self) -> uint {
    let mut cpy = self.data.clone();
    cpy.len()
  }
}

#[deriving(Eq, Clone)]
pub struct StrState {
    priv data: Rc<~str>,
    priv next: uint,
    priv pos: uint,
    priv size: uint,
}

impl StrState {
  fn new_at(&self, next: uint, pos: uint) -> StrState {
    StrState{ data: self.data.clone(), next: next, pos: pos, size: self.size }
  }

  pub fn new(data: ~str) -> StrState {
    let len = data.char_len();
    StrState{ data: Rc::new(data), next: 0, pos: 0, size: len }
  }

  pub fn take(&self, n: uint) -> Option<(~str, StrState)> {
    // Prevent index error on CharRange calculation by having a fastpath for n == 0
    if n == 0 {
      return Some((~"", self.clone()))
    }

    if self.len() < n {
      return None
    }

    let mut next = self.next;
    let mut i = n;

    while i > 0 {
      let char_range = self.data.borrow().char_range_at(next);

      next = char_range.next;

      i -= 1;
    }

    Some((self.data.borrow().slice(self.next, next).into_owned(), self.new_at(next, self.pos+n)))
  }

  pub fn content(&self) -> ~str {
    self.data.borrow().slice(self.pos, self.data.borrow().len()).into_owned()
  }
}

impl State<char> for StrState {
  fn head(&self) -> Option<(char, StrState)> {
    if self.len() > 0 {
      let CharRange {ch, next} = self.data.borrow().char_range_at(self.next);

      Some((ch, self.new_at(next, self.pos+1)))
    } else {
      None
    }
  }

  fn skip(&self, n: uint) -> Option<StrState> {
    self.take(n).map(|(_, inp)| inp)
  }

  fn len(&self) -> uint {
    self.size - self.pos
  }
}


#[cfg(test)]
mod test_vec_state {
  use super::{State, VecState};

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

#[cfg(test)]
mod test_iter_state {
  use super::{State, IterState};

  #[test]
  fn head() {
    let values = ~[1, 2, 3];
    let (data, rest) = IterState::new(values.iter()).head().unwrap();

    assert_eq!(data, &1);
    assert_eq!(rest.len(), 2);

    let (data, rest) = rest.head().unwrap();

    assert_eq!(data, &2);

    let (data, rest) = rest.head().unwrap();

    assert_eq!(data, &3);
    assert!(rest.head().is_none());
  }

  #[test]
  fn head_twice() {
    let values = ~[1, 2, 3];
    let input = IterState::new(values.iter());

    let (head1, _) = input.head().unwrap();
    let (head2, _) = input.head().unwrap();

    assert_eq!(head1, &1);
    assert_eq!(head2, &1);
  }

  #[test]
  fn take() {
    let values = ~[1, 2, 3];
    let expected = ~[&1, &2];
    let (data, rest) = IterState::new(values.iter()).take(2).unwrap();

    assert_eq!(data, expected);
    assert_eq!(rest.len(), 1);

    let (data, rest) = rest.take(1).unwrap();
    let expected = ~[&3];

    assert_eq!(data, expected);
    assert_eq!(rest.len(), 0);
    assert!(rest.take(2).is_none());
  }

  #[test]
  fn skip(){
    let values = ~[1, 2, 3];
    let rest = IterState::new(values.iter()).skip(2).unwrap();

    assert_eq!(rest.len(), 1);

    let rest = rest.skip(1).unwrap();

    assert_eq!(rest.len(), 0);
    assert!(rest.skip(2).is_none());
  }

  #[test]
  fn content() {
    let values = ~[1, 2, 3];
    assert_eq!(IterState::new(values.iter()).content().to_owned_vec(), values.iter().to_owned_vec());

    let values: ~[int] = ~[];
    assert_eq!(IterState::new(values.iter()).content().to_owned_vec(), values.iter().to_owned_vec());
  }
}

#[cfg(test)]
mod test_str_state {
  use super::{State, StrState};

  fn input() -> StrState {
     StrState::new(~"世界")
  }

  #[test]
  fn head() {
    let (data, rest) = input().head().unwrap();

    assert_eq!(data, '世');
    assert_eq!(rest.len(), 1);

    let (data, rest) = rest.head().unwrap();

    assert_eq!(data, '界');
    assert_eq!(rest.len(), 0);

    assert_eq!(rest.head(), None);
  }

  #[test]
  fn head_twice() {
    let input = input();

    let (head1, _) = input.head().unwrap();
    let (head2, _) = input.head().unwrap();

    assert_eq!(head1, '世');
    assert_eq!(head2, '世');
  }

  #[test]
  fn take() {
    let (data, rest) = input().take(2).unwrap();

    assert_eq!(data, ~"世界");
    assert_eq!(rest.len(), 0);

    let (data, rest) = rest.take(0).unwrap();
    assert_eq!(data, ~"");
    assert_eq!(rest.len(), 0);

    assert_eq!(rest.take(2), None);
  }

  #[test]
  fn skip(){
    let rest = input().skip(1).unwrap();

    assert_eq!(rest.len(), 1);

    let (data, rest) = rest.head().unwrap();

    assert_eq!(data, '界');
    assert_eq!(rest.len(), 0);

    assert_eq!(rest.skip(2), None);
  }

  #[test]
  fn content() {
    assert_eq!(input().content(), ~"世界");
    assert_eq!(StrState::new(~"").content(), ~"");
  }
}
