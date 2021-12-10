#![allow(unused_imports)]
use std::convert::identity;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day10 {
	lines: Vec<String>,
}
impl Day10 {
}

#[derive(Debug, Clone)]
enum LineStatus {
	Legal,
	Incomplete { expected: String },
	Corrupted { found: char },
}
impl LineStatus {
	fn is_incomplete(&self) -> bool { matches!(self, LineStatus::Incomplete { .. }) }
	fn is_corrupted(&self) -> bool { matches!(self, LineStatus::Corrupted { .. }) }
	fn score_owned(self) -> usize { self.score() }
	fn score(&self) -> usize {
		match self {
			LineStatus::Legal => 0,
			LineStatus::Incomplete { expected } => {
				expected.chars()
					.rev()
					.fold(0, |acc, ch| {
						match ch {
							')' => acc*5 + 1,
							']' => acc*5 + 2,
							'}' => acc*5 + 3,
							'>' => acc*5 + 4,
							_ => panic!("unexpected character in expected items: {:?}", ch),
						}
					})
			},
			LineStatus::Corrupted { found: ')' } => 3,
			LineStatus::Corrupted { found: ']' } => 57,
			LineStatus::Corrupted { found: '}' } => 1197,
			LineStatus::Corrupted { found: '>' } => 25137,
			LineStatus::Corrupted { found: c } => panic!("bad corrupted line status: found invalid character {:?}", c),
		}
	}
	fn compute(line: &str) -> LineStatus {
		let mut expected_stack = String::with_capacity(line.len()/2);

		for found in line.chars() {
			match found {
				'(' => expected_stack.push(')'),
				'[' => expected_stack.push(']'),
				'{' => expected_stack.push('}'),
				'<' => expected_stack.push('>'),
				')' | ']' | '}' | '>' => {
					let expected = expected_stack.pop();
					if expected != Some(found) {
						return LineStatus::Corrupted {
							found,
						};
					}
				},
				c => panic!("unexpected character for line: {:?}", c),
			}
		}

		if expected_stack.is_empty() {
			LineStatus::Legal
		} else {
			LineStatus::Incomplete {
				expected: expected_stack,
			}
		}
	}
}

impl AoCDay for Day10 {
	type Answer = usize;

	fn day() -> u8 { 10 }
	fn name() -> &'static str { "Syntax Scoring" }

	fn parse(input: &str) -> DayResult<Self> {
		Ok(Day10 {
			lines: input
				.lines()
				.filter_map(aoch::parsing::trimmed)
				.map(str::to_owned)
				.collect(),
		})
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		Ok(self.lines.iter()
			.map(String::as_ref)
			.map(LineStatus::compute)
			.filter(LineStatus::is_corrupted)
			.map(LineStatus::score_owned)
			.sum()
		)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut scores: Vec<usize> = self.lines.iter()
			.map(String::as_ref)
			.map(LineStatus::compute)
			.filter(LineStatus::is_incomplete)
			.map(LineStatus::score_owned)
			.collect();
		scores.sort_unstable();
		Ok(scores[scores.len()/2])
	}
}

#[test]
fn fuel_calc() {
	let cases = [
		("}}]])})]", 288957),
		(")}>]})", 5566),
		("}}>}>))))", 1480781),
		("]]}}]}]}>", 995444),
		("])}>", 294),
	];
	run_test(|n| {
		let ls = LineStatus::Incomplete {
			expected: n.chars().rev().collect(),
		};
		ls.score()
	}, &cases);
}


#[cfg(test)]
const TEST_INPUT: &'static str = "
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 26397),
		(daystr!("10"), 369105),
	];
	test_runner::<Day10, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 288957),
		(daystr!("10"), 3999363569),
	];
	test_runner::<Day10, _>(DayPart::Part2, &cases);
}
