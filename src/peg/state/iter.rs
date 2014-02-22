use state::State;

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

#[cfg(test)]
mod test_iter_state {
  use state::{State};
  use super::{IterState};

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

