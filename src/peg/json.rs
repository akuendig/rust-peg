use peg::{Parser, TupleParser, anyOf, AnyOf, chr, ZeroOrMore};
use state::StrState;

mod peg;
mod state;

static WS_CHARS: ~[char] = ~['\x20', '\x09', '\x0A', '\x0D'];

pub fn json() -> ~Parser<~str, StrState<'static>> {
  let ws_char: AnyOf<char> = anyOf(WS_CHARS);
  //let ws = chr('\x20').or(chr('\x09')).or(chr('\x0A')).or(chr('\x0D')).zeroOrMore();
  let ws: ZeroOrMore<AnyOf<char>> = ws_char.zeroOrMore();
  let begin_array = ws.and(chr('\x5B')).and(ws).drop_right().drop_left();
  let end_array = ws.and(chr('\x5D')).and(ws).drop_right().drop_left();
  let begin_object = ws.and(chr('\x7B')).and(ws).drop_right().drop_left();
  let end_object = ws.and(chr('\x7D')).and(ws).drop_right().drop_left();
  let JSON_text = begin_array.and(end_array);

  ~JSON_text as ~Parser<~str, StrState<'static>>
}

#[cfg(test)]
mod test_json {
  use super::json;
  use state::StrState;

  #[test]
  fn test_first() {
    let p = json();
    let res = p.run(StrState::new("Hello World"));

    assert!(res.get() == ~"Hello World");
  }
}
