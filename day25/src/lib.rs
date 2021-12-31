#![allow(unused_imports)]
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::ops::{Index, IndexMut};
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
	East,
	South,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Field {
	step: usize,
	width: usize,
	height: usize,
	data: Vec<Option<Direction>>,
}
impl Field {
	fn parse(input: &str) -> Field {
		let lines: Vec<&str> = input.trim().lines()
			.filter_map(aoch::parsing::trimmed)
			.collect();

		let height = lines.len();
		let width = lines[0].len();
		let mut data = vec![None; height*width];

		for (y, line) in lines.iter().enumerate() {
			for (x, c) in line.chars().enumerate() {
				match c {
					'v' => { data[y*width + x] = Some(Direction::South); },
					'>' => { data[y*width + x] = Some(Direction::East) },
					'.' => {},
					_ => panic!("unexpected input char: {:?}", c),
				}
			}
		}

		Field { step: 0, width, height, data }
	}

	fn as_coords(&self, i: usize) -> (usize, usize) {
		(i % self.width, i / self.width)
	}

	#[inline(always)]
	fn move_index(&self, i: usize, dir: Direction, rev: bool) -> usize {
		use Direction::*;

		let res = match (dir, rev) {
			(East, false) => {
				let mut n = i + 1;
				if n % self.width == 0 {
					// preserve line
					n -= self.width;
				}
				n
			},
			(East, true) => {
				if i == 0 {
					self.width-1
				} else {
					let mut n = i - 1;
					if n % self.width == self.width-1 {
						// preserve line
						n += self.width;
					}
					n
				}
			},
			(South, false) => {
				let mut n = i + self.width;
				if n >= self.data.len() {
					n -= self.data.len();
				}
				n
			},
			(South, true) => {
				let n = i.checked_sub(self.width)
					.unwrap_or(self.data.len() - self.width + i);
				n
			},
		};

		if cfg!(debug_assertions) {
			let (mut xs, mut ys) = self.as_coords(i);
			match (dir, rev) {
				(East, false) => xs += 1,
				(East, true) => xs = xs.checked_sub(1).unwrap_or(self.width-1),
				(South, false) => ys += 1,
				(South, true) => ys = ys.checked_sub(1).unwrap_or(self.height-1),
			};
			xs %= self.width;
			ys %= self.height;
			assert_eq!(
				(xs, ys), self.as_coords(res),
				"bad coordinate translation for i={}={:?}, dir={:?}, rev={}",
				i, self.as_coords(i), dir, rev
			);
		}

		res
	}

	fn step_naive(&self) -> Field {
		use Direction::*;
		let mut next = Field {
			step: self.step + 1,
			width: self.width,
			height: self.height,
			data: vec![None; self.data.len()],
		};

		assert!(next.data.len() == self.data.len());

		for (i, c) in self.data.iter().enumerate() {
			// figure out where they should go
			match c {
				None => {},
				Some(East) => {
					let dst = self.move_index(i, East, false);
					match self.data[dst] {
						None => next.data[dst] = Some(East), // we've moved
						_ => next.data[i] = Some(East), // we've not moved - replace us
					}
				},
				Some(South) => {
					let dst = self.move_index(i, South, false);
					match self.data[dst] {
						None if self.data[self.move_index(dst, East, true)] != Some(East) => {
							// an eastener did not beat us there - move
							next.data[dst] = Some(South);
						},
						Some(East) => {
							if self.data[self.move_index(dst, East, false)] == None {
								// an eastener is there - it has moved, so we move
								next.data[dst] = Some(South);
							} else {
								// an eastener is there - it hasn't moved - replace us
								next.data[i] = Some(South);
							}
						},
						None | Some(South) => {
							// we can't move - replace us
							next.data[i] = Some(South);
						}
					}
				},
			};
		}
		
		debug_assert_eq!(
			self.data.iter().copied().filter(Option::is_some).count(),
			next.data.iter().copied().filter(Option::is_some).count(),
			"fields do not have an equal number of sea cucumbers after step. \nold(S={}):\n{}\n\nnext(S={}):\n{}\n", self.step, self, next.step, next
		);

		next
	}
}
impl fmt::Display for Field {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for (i, c) in self.data.iter().enumerate() {
			f.write_char(match c {
				None => '.',
				Some(Direction::East) => '>',
				Some(Direction::South) => 'v',
			})?;

			if i % self.width == self.width-1 {
				f.write_char('\n')?;
			}
		}

		Ok(())
	}
}

pub struct Day25 {
	field: Field,
}
impl Day25 {
}

impl AoCDay for Day25 {
	type Answer = usize;

	fn day() -> u8 { 25 }
	fn name() -> &'static str { "Sea Cucumber" }

	fn parse(input: &str) -> DayResult<Self> {
		let field = Field::parse(input);

		Ok(Day25 { field })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {

		let mut last = self.field.clone();
		
		let mut i = 0;
		loop {

			let next = last.step_naive();
			i += 1;

			if next.data == last.data {
				return Ok(i);
			} else {
				last = next;
			}
		}
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		eprintln!("Day 25 has no part 2");
		Ok(0)
	}
}

#[cfg(test)]
fn match_states(cases: &[(&str, usize)]) {
	let mut state = Field::parse(cases[0].0);
	for (case, steps) in cases.iter() {
		while state.step < *steps {
			state = state.step_naive();
		}

		let mut parsed = Field::parse(case);
		parsed.step = *steps;

		if state.data != parsed.data {
			eprintln!("expected: {}, {}x{}", parsed.step, parsed.width, parsed.height);
			eprintln!("{}", parsed);
			eprintln!("");
			eprintln!("generated: {}, {}x{}", state.step, state.width, state.height);
			eprintln!("{}", state);
			
			panic!("steps {} did not match", state.step);
		}
	}
}

#[test]
fn steps_sparse() {
	let states: &[(&str, usize)] = &[
("...>...
.......
......>
v.....>
......>
.......
..vvv..", 0),
("..vv>..
.......
>......
v.....>
>......
.......
....v..", 1),
("....v>.
..vv...
.>.....
......>
v>.....
.......
.......", 2),
("......>
..v.v..
..>v...
>......
..>....
v......
.......", 3),
(">......
..v....
..>.v..
.>.v...
...>...
.......
v......", 4),
	];

	match_states(states);
}

#[test]
fn steps_dense() {
	let states: &[(&str, usize)] = &[
("v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>", 0),
("....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v", 1),
(">.v.v>>..v
v.v.>>vv..
>v>.>.>.v.
>>v>v.>v>.
.>..v....v
.>v>>.v.v.
v....v>v>.
.vv..>>v..
v>.....vv.", 2),
("v>v.v>.>v.
v...>>.v.v
>vv>.>v>..
>>v>v.>.v>
..>....v..
.>.>v>v..v
..v..v>vv>
v.v..>>v..
.v>....v..", 3),
("v>..v.>>..
v.v.>.>.v.
>vv.>>.v>v
>>.>..v>.>
..v>v...v.
..>>.>vv..
>.v.vv>v.v
.....>>vv.
vvv>...v..", 4)
	];

	match_states(states);
}

#[cfg(test)]
const TEST_INPUT_BUSY: &'static str = "
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT_BUSY, 58),
		(daystr!("25"), 435),
	];
	test_runner::<Day25, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(daystr!("25"), 0),
	];
	test_runner::<Day25, _>(DayPart::Part2, &cases);
}
