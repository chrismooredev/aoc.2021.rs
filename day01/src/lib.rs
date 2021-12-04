use aoch::{AoCDay, DayResult};
#[allow(unused_imports)] use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day01 {
	nums: Vec<isize>,
}
impl Day01 {
}

impl AoCDay for Day01 {
	type Answer = usize;

	fn day() -> u8 { 01 }
	fn name() -> &'static str { "Sonar Sweep" }

	fn parse(input: &str) -> DayResult<Self> {
		aoch::parsing::from_lines(input)	
			.map(|nums| Day01 { nums })
			.map_err(|e| e.into())
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut increased = 0;
		for i in 1..self.nums.len() {
			if self.nums[i-1] < self.nums[i] {
				increased += 1;
			}
		}
		Ok(increased)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut increased = 0;

		let windowed: Vec<isize> = self.nums.windows(3)
			.map(|s| s.iter().sum())
			.collect();

		for i in 1..windowed.len() {
			if windowed[i-1] < windowed[i] {
				increased += 1;
			}
		}

		Ok(increased)
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
199
200
208
210
200
207
240
269
260
263
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 7),
		(daystr!("01"), 1502),
	];
	test_runner::<Day01, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 5),
		(daystr!("01"), 1538),
	];
	test_runner::<Day01, _>(DayPart::Part2, &cases);
}
