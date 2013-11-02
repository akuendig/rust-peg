use std::str::{CharRange};

pub trait State<A> {
	fn head(&self) -> Option<(A, Self)>;
	fn skip(&self, n: uint) -> Option<Self>;
	fn len(&self) -> uint;
}

#[deriving(Eq, ToStr, Clone)]
pub struct VecState<'self, A> {
    priv data: &'self [A],
    priv pos: uint,
    priv size: uint,
}

impl<'self, A: Clone> VecState<'self, A> {
	fn new_at(&self, pos: uint) -> VecState<'self, A> {
		VecState{ data: self.data, pos: pos, size: self.size }
	}

	pub fn new(data: &'self [A]) -> VecState<'self, A> {
		VecState{ data: data, pos: 0, size: data.len() }
	}

	pub fn take(&self, n: uint) -> Option<(&'self [A], VecState<'self, A>)> {
		if self.len() >= n {
			Some((self.data.slice(self.pos, self.pos + n), self.new_at(self.pos + n)))
		} else {
			None
		}
	}

	pub fn content(&self) -> &'self [A] {
		self.data.slice(self.pos, self.size)
	}
}

impl<'self, A: Clone> State<A> for VecState<'self, A> {
	fn head(&self) -> Option<(A, VecState<'self, A>)> {
		if self.len() > 0 {
			Some((self.data[self.pos].clone(), self.new_at(self.pos + 1)))
		} else {
			None
		}
	}

	fn skip(&self, n: uint) -> Option<VecState<'self, A>> {
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

impl<A: Clone, T: Clone + Iterator<A>> State<A> for IterState<T> {
	fn head(&self) -> Option<(A, IterState<T>)> {
		let mut cpy = self.data.clone();

		let head = cpy.next();
		head.map(|c: A| (c, IterState{ data: cpy.clone() }))
	}

	fn skip(&self, n: uint) -> Option<IterState<T>> {
		let mut cpy = self.data.clone();

		for _ in range(0, n) {
			match cpy.next() {
				Some(*) => (),
				None => return None,
			}
		}

		Some(IterState{ data: cpy.clone() })
	}

	fn len(&self) -> uint {
		let mut cpy = self.data.clone();
		cpy.len()
	}
}

impl<A: Clone, T: Clone + Iterator<A>> IterState<T> {
	pub fn new(data: T) -> IterState<T> {
		IterState{ data: data }
	}

	pub fn take(&self, n: uint) -> Option<(~[A], IterState<T>)> {
		let mut cpy = self.data.clone();
		let mut slice: ~[A] = ~[];

		for _ in range(0, n) {
			match cpy.next() {
				Some(v) => slice.push(v),
				None => return None,
			}
		}

		Some((slice, IterState{ data: cpy.clone() }))
	}

	pub fn content(&self) -> T {
		self.data.clone()
	}
}

#[deriving(Eq, ToStr, Clone)]
pub struct StrState<'self> {
    priv data: &'self str,
    priv next: uint,
    priv pos: uint,
    priv size: uint,
}

impl<'self> StrState<'self> {
	fn new_at(&self, next: uint, pos: uint) -> StrState<'self> {
		// println(format!("{:?} {:?} {:?}", self.data, pos, self.size));
		// println(format!("{:?}", StrState{ data: self.data, pos: pos, size: self.size }));
		StrState{ data: self.data, next: next, pos: pos, size: self.size }
	}

	pub fn new(data: &'self str) -> StrState<'self> {
		StrState{ data: data, next: 0, pos: 0, size: data.char_len() }
	}

	pub fn take(&self, n: uint) -> Option<(&'self str, StrState<'self>)> {
		// Prevent index error on CharRange calculation by having a fastpath for n == 0
		if n == 0 {
			return Some((&"", self.clone()))
		}

		if self.len() < n {
			return None
		}

		let mut next = self.next;

		for _ in range(0, n) {
			let char_range = self.data.char_range_at(next);

			next = char_range.next;
		}

		Some((self.data.slice(self.pos, next), self.new_at(next, self.pos+n)))
	}

	pub fn content(&self) -> &'self str {
		self.data.slice(self.pos, self.data.len())
	}
}

impl<'self> State<char> for StrState<'self> {
	fn head(&self) -> Option<(char, StrState<'self>)> {
		if self.len() > 0 {
			let CharRange {ch, next} = self.data.char_range_at(self.next);

			Some((ch, self.new_at(next, self.pos+1)))
		} else {
			None
		}
	}

	fn skip(&self, n: uint) -> Option<StrState<'self>> {
		self.take(n).map(|(_, inp)| inp)
	}

	fn len(&self) -> uint {
		self.size - self.pos
	}
}


#[cfg(test)]
mod test_vec_state {
	use super::VecState;

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

		assert_eq!(data, &[1, 2]);
		assert_eq!(rest.len(), 1);

		let (data, rest) = rest.take(1).unwrap();

		assert_eq!(data, &[3]);
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
		assert_eq!(VecState::new(values).content(), &[1, 2, 3]);

		let values: ~[int] = ~[];
		assert_eq!(VecState::new(values).content(), &[]);
	}
}

#[cfg(test)]
mod test_iter_state {
	use super::IterState;

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
	use super::StrState;

	fn input() -> StrState {
		 StrState::new("世界")
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

		assert_eq!(data, &"世界");
		assert_eq!(rest.len(), 0);

		let (data, rest) = rest.take(0).unwrap();
		assert_eq!(data, &"");
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
		assert_eq!(input().content(), &"世界");
		assert_eq!(StrState::new("").content(), &"");
	}
}
