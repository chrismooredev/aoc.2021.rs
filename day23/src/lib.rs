#![feature(generic_const_exprs)]
#![feature(int_abs_diff)]

#![allow(const_evaluatable_unchecked)]
#![allow(incomplete_features)]
#![allow(clippy::needless_range_loop)]

use std::cmp::Ordering;
use std::ops::Add;
use std::time::Instant;
use itertools::Itertools;

use aoch::{AoCDay, DayResult};
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayError, DayPart, run_test, test_runner, daystr};

pub use crate::impl2::{GameState2, Pod, Move};
pub mod impl2;

type MCN = u32;

/// A raw count of totals for how many spaces each amphipod moved. Note that this is /not/ the score of those moves.
#[derive(Debug, Clone, Copy, Eq)]
pub struct MoveCount([MCN; 4]);
impl MoveCount {
	const ZERO: MoveCount = MoveCount([0, 0, 0, 0]);
	pub fn sum(&self) -> usize {
		let a = self.0[0] as usize;
		let b = self.0[1] as usize*10;
		let c = self.0[2] as usize*100;
		let d = self.0[3] as usize*1000;
		a + b + c + d
	}
}
impl Add<Move> for MoveCount {
	type Output = MoveCount;
	fn add(mut self, rhs: Move) -> Self::Output {
		rhs.assert_length();
		self.0[rhs.pod() as usize] += rhs.len() as MCN;
		self
	}
}
impl PartialEq for MoveCount {
	fn eq(&self, other: &Self) -> bool {
        self.sum() == other.sum()
    }
}
impl PartialOrd for MoveCount {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.sum().partial_cmp(&other.sum())
	}
}
impl FromIterator<Move> for MoveCount {
	fn from_iter<T: IntoIterator<Item = Move>>(iter: T) -> Self {
		let mut counts = MoveCount([0, 0, 0, 0]);
		for mov in iter.into_iter() {
			mov.assert_length();
			counts.0[mov.pod() as usize] += mov.len() as MCN;
		}
		counts
	}
}


pub struct Day23 {
	/// Pods in order of top row, bottom row 
	pods: [Pod; 8],
}
impl Day23 {
	fn run_with_logging<const D: usize>(rooms: [[Pod; D]; 4]) -> usize {
		let initial = GameState2::<D, 4>::from_solid(rooms);
		eprintln!("gamestate2: parsed");
		eprintln!("{}", initial);

		eprintln!("starting to find solutions...");
		let mut sols = initial.solutions();
		let mut i = 0;
		let start = Instant::now();
		let mut best = usize::MAX;
		let minmax = (&mut sols)
			.inspect(|moved| {
				i += 1;
				if moved.sum() < best {
					best = moved.sum();
					let dur = Instant::now() - start;
					eprintln!("\t[{}s] new best @ i={} :: {} {:?}", dur.as_secs(), i, best, moved);
				}
			})
			.minmax_by_key(|moved| moved.sum());
		let dur = Instant::now()-start;

		eprintln!("unique states: {:?}", sols.found.len());
		eprintln!("total iterations: {}, minmax: {:?}", i, minmax);
		let (min, max) = minmax.into_option().unwrap();
		eprintln!("min/max score: {}/{}", min.sum(), max.sum());
		
		eprintln!("ran for {}s (or {:.02}m or {:.02}h)", dur.as_secs(), dur.as_secs_f64()/60.0, dur.as_secs_f64()/60.0/60.0);

		min.sum()
	}
}

impl AoCDay for Day23 {
	type Answer = usize;

	fn day() -> u8 { 23 }
	fn name() -> &'static str { "Amphipod" }

	fn parse(input: &str) -> DayResult<Self> {
		let places: Vec<Pod> = input.chars()
			.filter(|c| c.is_alphabetic())
			.map(|c| Pod::try_from_char(c).expect("unexpected input letter").expect("bottom should be solid 'pods"))
			.collect();

		Ok(Day23 {
			pods: places.try_into().expect("expected exactly 8 amphipods"),
		})
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		macro_rules! print_size {
			($t: ty) => {
				eprintln!(concat!(stringify!($t), " size: {}"), std::mem::size_of::<$t>());		
			}
		}
		print_size!(Pod);
		print_size!(Option<Pod>);
		print_size!(MoveCount);
		print_size!(GameState2<2, 4>);
		print_size!((GameState2<2, 4>, MoveCount));
		print_size!(GameState2<4, 4>);
		print_size!((GameState2<4, 4>, MoveCount));

		let rows: [&[Pod]; 2] = [
			&self.pods[0..4],
			&self.pods[4..8],
		];

		let mut rooms = [[Pod::A; 2]; 4];
		for d in 0..2 {
			for room in 0..4 {
				rooms[room][d] = Pod::try_from_char(rows[d][room].chr()).unwrap().unwrap();
			}
		}

		Ok(Day23::run_with_logging(rooms))
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let rows: [&[Pod]; 4] = [
			&self.pods[0..4],
			&[Pod::D, Pod::C, Pod::B, Pod::A],
			&[Pod::D, Pod::B, Pod::A, Pod::C],
			&self.pods[4..8],
		];

		let mut rooms = [[Pod::A; 4]; 4];
		for d in 0..4 {
			for room in 0..4 {
				rooms[room][d] = rows[d][room];
			}
		}

		Ok(Day23::run_with_logging(rooms))
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 12521), // 0s on release mode on i7-4790k @ 4.00GHz (1s total search)
		(daystr!("23"), 14627), // 16s on release mode on iu-4790k @ 4.00GHz (30s total search)
	];
	test_runner::<Day23, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 44169), // 28s on release mode on iu-4790k @ 4.00GHz (28s total search)
		(daystr!("23"), 41591), // 5s on release mode on iu-4790k @ 4.00GHz (540s total search)
	];
	test_runner::<Day23, _>(DayPart::Part2, &cases);
}
