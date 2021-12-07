use std::num::ParseIntError;

use aoch::{AoCDay, DayResult};
#[allow(unused_imports)] use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};
use itertools::Itertools;

pub struct Day07 {
	positions: Vec<isize>,
}
impl Day07 {
	fn fuel_cost_linear(&self, pos: isize) -> usize {
		self.positions.iter()
			.map(|&n| if n > pos { n - pos } else { pos - n })
			.sum::<isize>() as usize
	}
	fn fuel_cost_exponential(&self, pos: isize) -> usize {
		// O(n) = O(n*1) - if `n*(n+1)/2` is used
		// O(n^2) = O(n*n) - if `(0..=n).sum()` is used
		self.positions.iter()
			.map(|&n| if n > pos { n - pos } else { pos - n })
			.map::<isize, _>(|n| n*(n+1)/2) 
			// .map::<isize, _>(|n| (0..=n).sum())
			.sum::<isize>() as usize
	}

	/// Loops through [min, max] to find the lowest cost
	/// 
	/// Originally used a heuristic involving spiraling around the average,
	/// and stopping after so many cycles of no new bests. Turns out this isn't significantly slower.
	fn get_best_simple<F: Fn(&Day07, isize) -> usize>(&self, f: F) -> usize {
		// go from min..max and find the minimum fuel cost possible
		let (&min, &max) = self.positions.iter().minmax().into_option().unwrap();

		(min..max)
			.map(|n| f(&self, n))
			.min().unwrap() as usize
	}
}

impl AoCDay for Day07 {
	type Answer = usize;

	fn day() -> u8 { 07 }
	fn name() -> &'static str { "The Treachery of Whales" }

	fn parse(input: &str) -> DayResult<Self> {
		let positions: Vec<_> = input.trim()
			.split(',')
			.map(str::parse)
			.collect::<Result<_, ParseIntError>>()?;

		Ok(Day07 { positions })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		Ok(self.get_best_simple(Day07::fuel_cost_linear))
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		Ok(self.get_best_simple(Day07::fuel_cost_exponential))
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "16,1,2,0,4,2,7,1,2,14";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 37),
		(daystr!("07"), 342730),
	];
	test_runner::<Day07, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 168),
		(daystr!("07"), 92335207),
	];
	test_runner::<Day07, _>(DayPart::Part2, &cases);
}
