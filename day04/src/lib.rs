use std::num::ParseIntError;

use aoch::{AoCDay, DayResult};
#[allow(unused_imports)] use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day04 {
	/// The raw call numbers
	calls: Vec<BoardNum>,
	/// Each board in play
	boards: Vec<Board>,
}
impl Day04 {
}

type BoardNum = u8;

#[derive(Clone, Copy)]
struct Board {
	// easier to reason about if it's flat
	nums: [(BoardNum, bool); 5*5],
}
impl Board {
	/// Marks a tile as hit.
	/// 
	/// Returns true if this causes a bingo to be won
	fn mark(&mut self, num: BoardNum) -> bool {
		let pos = self.nums.iter()
			.position(|n| n.0 == num);
		
		if let Some(i) = pos {
			self.nums[i].1 = true;

			let row_base = (i/5)*5; // integer math, round to lower multiple of 5
			let row = self.nums[row_base..row_base+5].iter();
			let column = (0..5)
				.map(|scale| (i%5) + scale*5)
				.map(|i| self.nums[i]);

			if row.filter(|n| n.1).count() == 5 {
				return true;
			}
			if column.filter(|n| n.1).count() == 5 {
				return true;
			}
		}
		false
	}
	fn score(&self) -> usize {
		self.nums.iter()
			.filter(|(_, active)| !active)
			.map(|(n, _)| *n as usize)
			.sum()
	}
}

impl AoCDay for Day04 {
	type Answer = usize;

	fn day() -> u8 { 4 }
	fn name() -> &'static str { "Giant Squid" }

	fn parse(input: &str) -> DayResult<Self> {
		let mut groups = input
			.split_terminator("\n\n")
			.filter_map(aoch::parsing::trimmed);

		let calls: Vec<BoardNum> = groups.next().expect("first group should be calls")
			.split(',')
			.map(str::parse)
			.collect::<Result<_, ParseIntError>>()?;

		let boards: Vec<Board> = groups
			.map(|s| {
				let ns: Vec<(BoardNum, bool)> = s.split('\n')
					.map(|l| l.split(' '))
					.flatten()
					.filter_map(aoch::parsing::trimmed)
					.map(str::parse)
					.map(|r| r.map(|n| (n, false)))
					.collect::<Result<_, ParseIntError>>()?;

				let hs: std::collections::HashSet<BoardNum> = ns.iter().map(|n| n.0).collect();
				assert_eq!(hs.len(), ns.len(), "numbers on board are non-unique!");

				Ok(ns.try_into().expect("each board should have 25 elements (5 columns, 5 rows"))
			})
			.map(|rb| rb.map(|nums| Board { nums }))
			.collect::<Result<_, ParseIntError>>()?;

		Ok(Day04 {
			calls,
			boards,
		})
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {

		for n in &self.calls {
			for board in &mut self.boards {
				if board.mark(*n) {
					return Ok(board.score() * (*n as usize));
				}
			}
		}

		Err(DayError::generic("no winners after all numbers have been called"))
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let boards_len = self.boards.len();
		let mut winner_order: Vec<usize> = Vec::with_capacity(boards_len);
		for n in &self.calls {
			for (i, board) in self.boards.iter_mut().enumerate() {
				if ! winner_order.contains(&i) && board.mark(*n) {
					// if board.insert(*n) {
						winner_order.push(i);

						if winner_order.len() == boards_len {
							// this board is the last to win
							return Ok(board.score() * (*n as usize));
						}
					// }
				}
			}
		}

		Err(DayError::generic("no winners after all numbers have been called"))
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 4512),
		(daystr!("04"), 60368),
	];
	test_runner::<Day04, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 1924),
		(daystr!("04"), 17435),
	];
	test_runner::<Day04, _>(DayPart::Part2, &cases);
}
