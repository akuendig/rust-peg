use parser::{Parser, ParseResult, Success, Failure};
use state::{State};

#[deriving(Eq, ToStr, Clone)]
pub struct Accept<A> {
  priv res: A
}

impl<A: Clone, T, S: State<T>> Parser<A, S> for Accept<A> {
  fn run(&self, input: S) -> ParseResult<A, S> {
    Success(self.res.clone(), input)
  }
}

pub fn success<A: Clone>(result: A) -> Accept<A> {
  Accept{ res: result }
}

#[deriving(Eq, ToStr, Clone)]
pub struct Peek;

impl<A: Clone, S: State<A>> Parser<A, S> for Peek {
  fn run(&self, input: S) -> ParseResult<A, S> {
    match input.head() {
      Some((c, inp)) => Success(c, inp),
      None => Failure(format!("Could not match char because EOI.")),
    }
  }
}

pub fn peek() -> Peek {
  Peek
}

#[deriving(Eq, ToStr, Clone)]
pub struct Elem<A> {
  priv e: A
}

impl<A: Clone + Eq, S: State<A>> Parser<A, S> for Elem<A> {
  fn run(&self, input: S) -> ParseResult<A, S> {
    match input.head() {
      Some((head, inp)) =>
        if head == self.e { Success(self.e.clone(), inp) }
        else { Failure(format!("Could not find matching char. found: {:?} required: {:?}", head, self.e)) },
      None => Failure(format!("Could not match char {:?} because EOI.", self.e)),
    }
  }
}

pub fn elem<A: Clone + Eq>(e: A) -> Elem<A> {
  Elem{ e: e }
}

#[deriving(Eq, ToStr, Clone)]
pub struct Elems<A> {
  priv elems: ~[A]
}

impl<A: Clone + Eq, S: State<A>> Parser<~[A], S> for Elems<A> {
  fn run(&self, input: S) -> ParseResult<~[A], S> {
    let mut state = input.clone();

    for elem in self.elems.iter() {
      match state.head() {
        Some((ref elem_, ref state_)) if elem == elem_ => state = state_.clone(),
        Some((ref elem_, _)) =>
          return Failure(format!("Could not find matching string. found: {:?} required: {:?}", elem_, elem)),
        None =>
          return Failure(format!("Could not match string {:?} because EOI.", self.elems)),
      }
    }

    Success(self.elems.clone(), state)
  }
}

pub fn elems<A: Clone + Eq>(elems: ~[A]) -> Elems<A> {
  Elems{ elems: elems }
}

#[cfg(test)]
mod tests {
  use parser::{Parser};
  use parser::generic::{success, peek};
  use state::{StrState};

  fn input() -> StrState {
     StrState::new(~"Hello, 世界")
  }

  #[test]
  fn test_success() {
    let (_, rest) = success(()).run(input()).unwrap();

    assert_eq!(rest.content(), ~"Hello, 世界");
  }

  #[test]
  fn test_peek() {
    let (head, rest) = peek().run(input()).unwrap();

    assert_eq!(head, 'H');
    assert_eq!(rest.content(), ~"ello, 世界");

    let failure_eof = peek().run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match char because EOI.");
  }
}
