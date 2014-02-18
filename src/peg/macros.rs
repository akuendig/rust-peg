use std::str::CharIterator;

struct MacroParserState <'a> {
  input: & 'a CharIterator<'a>
}

//macro_rules! one()
