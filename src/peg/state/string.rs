use std::rc::{Rc};
use std::str::{CharRange};

use state::State;

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
mod test_str_state {
  use state::{State};
  use super::{StrState};

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

