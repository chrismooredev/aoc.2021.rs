#![allow(unused_imports)]
use std::collections::HashMap;
use std::convert::identity;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

/// Asserts that the number of bits in the second argument are set in the first argument.
/// 
/// If no second argument is supplied, the first argument name's length is used as the expected length.
macro_rules! check_bits {
	($var: ident, $exp: literal) => {
		debug_assert!($var.count_ones() == $exp, "expected {} set bits for '{}', got {} ({:07b})", $exp, stringify!($var), $var.count_ones(), $var);
	};
	($var: ident) => {
		let exp = stringify!($var);
		debug_assert!($var.count_ones() as usize == exp.len(), "expected {} set bits for '{}', got {} ({:07b})", exp.len(), stringify!($var), $var.count_ones(), $var);
	};
}


pub struct Day08 {
	signals: Vec<Signals>,
}
impl Day08 {
	
}

struct Signals {
	/// Unordered available patterns
	patterns: [u8; 10],
	/// Ordered output display
	output: [u8; 4],
}
impl Signals {
	const UNIQ_LENS: [u8; 4] = [2, 4, 3, 7]; // 1 4 7 8

	fn uniques(&self) -> usize {
		self.output.iter()
			.filter(|s| Signals::UNIQ_LENS.contains(&(s.count_ones() as u8)))
			.count()
	}
	fn solve(patterns: &[u8; 10]) -> [u8; 10] {
		fn find_uniq<F: FnMut(u8) -> bool>(arr: &[u8], set_bits: u32, mut pred: F) -> u8 {
			arr.iter()
				.filter(|&&p| pred(p))
				.find(|b| b.count_ones() == set_bits)
				.copied()
				.unwrap()
		}

		// get the numbers with unique set bit counts
		let v1 = find_uniq(patterns, 2, |_| true);
		let v4 = find_uniq(patterns, 4, |_| true);
		let v7 = find_uniq(patterns, 3, |_| true);
		let v8 = find_uniq(patterns, 7, |_| true);

		let bd = (v4 | v7) ^ v7;         check_bits!(bd);

		// 2 3 5 -> 5
		let v5 = find_uniq(patterns, 5, |p| p & bd == bd);

		let e = !(v5 | v1) & v8;        check_bits!(e);
		let v9 = v8 ^ e;                check_bits!(v9, 6);
		
		let f = v5 & v1;                check_bits!(f);
		let abd = (v4 | v7) ^ v1;       check_bits!(abd);
		let eg =  (v4 | v7) ^ v8;       check_bits!(eg);
		let v6 = abd | eg | f;          check_bits!(v6, 6);

		
		// 0 6 9 -> 0
		let v0 = find_uniq(patterns, 6, |p| p != v6 && p != v9);
			
		let b = v0 & bd;                check_bits!(b);
		let v2 = v8 ^ b ^ f;            check_bits!(v2, 5);
		let v3 = v8 ^ b ^ e;            check_bits!(v3, 5);

		[v0, v1, v2, v3, v4, v5, v6, v7, v8, v9]
	}
	fn display_value(&self) -> usize {
		let key = Signals::solve(&self.patterns);

		let mut num = 0;
		for (place, display_lines) in self.output.iter().rev().enumerate() {
			for (digit, digit_val) in key.iter().enumerate() {
				if digit_val == display_lines {
					num += digit * usize::pow(10, place as u32);
					break;
				}
			}
		}
		num
	}
}

#[derive(Debug, thiserror::Error)]
#[error("error validating signal input")]
enum SignalValidationError {
	LineFormat,
	PatternLength,
	OutputLength,
}
impl FromStr for Signals {
	type Err = SignalValidationError;
	// input validation
	fn from_str(line: &str) -> Result<Self, Self::Err> {
		// split into groups
		let (patterns, output) = line.split_once(" | ").ok_or(SignalValidationError::LineFormat)?;

		// isolate each pattern, normalize by sorting chars
		let patterns: [u8; 10] = patterns.split(' ')
			.map(|s| {
				s.chars()
					.map(|c| c as u8 - b'a')
					.fold(0, |acc, c| acc | 1 << c)
			})
			.collect::<Vec<_>>()
			.try_into()
			.map_err(|_| SignalValidationError::PatternLength)?;
		let output: [u8; 4] = output.split(' ')
			.map(|s| {
				s.chars()
					.map(|c| c as u8 - b'a')
					.fold(0, |acc, c| acc | 1 << c)
			})
			.collect::<Vec<_>>()
			.try_into()
			.map_err(|_| SignalValidationError::OutputLength)?;

		Ok(Signals {
			patterns,
			output,
		})
	}
}

impl AoCDay for Day08 {
	type Answer = usize;

	fn day() -> u8 { 8 }
	fn name() -> &'static str { "Seven Segment Search" }

	fn parse(input: &str) -> DayResult<Self> {
		aoch::parsing::from_lines(input)
			.map(|signals| Day08 { signals })
			.map_err(DayError::boxed)
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let uniqs = self.signals.iter()
			.map(Signals::uniques)
			.sum();
		Ok(uniqs)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		Ok(self.signals.iter()
			.map(Signals::display_value)
			.sum())
	}
}

#[cfg(test)]
const TEST_INPUT_SMALL: &'static str = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";

#[cfg(test)]
const TEST_INPUT: &'static str = "
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT_SMALL, 0), // no simples
		(TEST_INPUT, 26),
		(daystr!("08"), 470),
	];
	test_runner::<Day08, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 61229),
		(daystr!("08"), 989396),
	];
	test_runner::<Day08, _>(DayPart::Part2, &cases);
}

 
