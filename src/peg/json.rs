use parser::{Parser, TupleParser};
use parser::string::{anyOf, chr, ConcatenableParser};
use state::{StrState};

mod parser;
mod state;

static WS_CHARS: &'static str = "\x20\x09\x0A\x0D";

pub fn json() -> ~Parser<~str, StrState> {
  let ws_char = anyOf(WS_CHARS.into_owned());
  // The next statement will give a compiler error. This is because
  // AnyOf<char> is a Parser<char, _>, where _ is undefined. The problem
  // is now that the compiler is not strong enough to carry on this information.
  // We either have to define the state, give it as parameter to anyOf, or
  // wait for the compiler to catch up.
  let ws = ws_char.many();
  let begin_array = ws.and(chr('\x5B')).and(ws.clone()).drop_right().drop_left();
  let end_array = ws.and(chr('\x5D')).and(ws.clone()).drop_right().drop_left();
  let array = begin_array.and(end_array);
  let begin_object = ws.and(chr('\x7B')).and(ws.clone()).drop_right().drop_left();
  let end_object = ws.and(chr('\x7D')).and(ws.clone()).drop_right().drop_left();
  let object = begin_object.and(end_object);
  let string =
    ws.and(chr('"'))
      .and(anyOf(~"Helo Wrd").many())
      .drop_left()
      .and(chr('"'))
      .drop_right()
      .concat();

  fn TODO((leftBracket, rightBracket): (char, char)) -> ~str { ~"TODO" }
  let JSON_text = (array.or(object)).map(TODO).or(string);

  ~JSON_text as ~Parser<~str, StrState>
}

#[cfg(test)]
mod test_json {
  use super::json;
  use state::StrState;

  #[test]
  fn test_first() {
    let p = json();
    let res = p.run(StrState::new(~"\"Hello World\""));

    assert!(res.get() == ~"Hello World");
  }
}
