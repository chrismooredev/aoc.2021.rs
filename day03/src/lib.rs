#![feature(drain_filter)]

use std::cmp::Ordering;
use std::num::ParseIntError;

use aoch::{AoCDay, DayResult};
#[allow(unused_imports)] use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day03 {
	words: Vec<usize>,
	wordlen: usize,
}
impl Day03 {
	/// Figure out the most common bit in a 'column' of integers
	fn common_bit(words: &[usize], bit_index: usize) -> Ordering {
		let set_count = words.iter()
			.filter(|&&w| w & (1 << bit_index) != 0)
			.count();
		
		let half = (words.len()+1)/2; // ensure we round up, if applicable
		set_count.cmp(&half)
	}

	fn get_rating(&self, typ: BitCriteria) -> usize {
		let mut candidates = self.words.clone();
	
		for bi in (0..self.wordlen).rev() {
			// get the commonality for this bit
			let target_state = typ.should_be_set(Day03::common_bit(&candidates, bi));

			// explicitly drop before end of scope so length break check works properly
			let _ = candidates.drain_filter(|&mut word| {
				(word & (1 << bi) != 0) != target_state
			});

			if candidates.len() == 1 {
				break;
			}
		}
		
		assert_eq!(candidates.len(), 1);
		candidates[0]
	}
}

#[derive(Debug, Clone, Copy)]
enum BitCriteria {
	Oxygen,
	Co2,
}
impl BitCriteria {
	/// Determines the target state of a bit, provided which machine (Oxygen/CO2) and the
	/// commonality (Greater=mostly 1s, Less=mostly 0s, Equal) of the bit total for this column.
	fn should_be_set(&self, commonality: Ordering) -> bool {
		use BitCriteria::*;
		use Ordering::*;

		match self {
			Oxygen => matches!(commonality, Greater | Equal),
			Co2 => matches!(commonality, Less)
		}
	}
}

impl AoCDay for Day03 {
	type Answer = usize;

	fn day() -> u8 { 03 }
	fn name() -> &'static str { "Binary Diagnostic" }

	fn parse(input: &str) -> DayResult<Self> {
		let mut wordlen: Option<usize> = None;
		let raw: Result<Vec<usize>, ParseIntError> = input.lines()
			.filter_map(aoch::parsing::trimmed)
			.map(|w| {
				match wordlen {
					None => wordlen = Some(w.len()),
					Some(r) => assert_eq!(r, w.len()),
				};

				// will return Err if number is too big for usize
				usize::from_str_radix(w, 2)	
			})
			.collect();
		
		let words = raw?;
		let wordlen = wordlen.unwrap();
		assert!(words.len() % 2 == 0, "non-even number of input elements, got {} elements", words.len());
		
		Ok(Day03 {
			words,
			wordlen: wordlen,
		})
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let bit_commonality: Vec<Ordering> = (0..self.wordlen)
			.map(|bi| Day03::common_bit(&self.words, bi))
			.collect();

		let gamma = bit_commonality.iter().enumerate()
			.fold(0, |gamma, (bi, bo)| {
				match bo {
					Ordering::Equal => panic!("bits are equally as common in column {}", bi),
					Ordering::Greater => gamma | (1 << (bi)),
					Ordering::Less => gamma,
				}
			});

		let wordmask = (1 << self.wordlen) - 1;
		let epsilon = gamma ^ wordmask;

		Ok(gamma * epsilon)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let oxy = self.get_rating(BitCriteria::Oxygen);
		let co2 = self.get_rating(BitCriteria::Co2);

		Ok(oxy * co2)
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 198),
		(daystr!("03"), 3895776),
	];
	test_runner::<Day03, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 230),
		(daystr!("03"), 7928162),
	];
	test_runner::<Day03, _>(DayPart::Part2, &cases);
}
