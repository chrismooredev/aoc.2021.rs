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
	fn common_bit(words: &[usize], bit_index: usize) -> Ordering {
		let set_count = words.iter()
			.filter(|&&w| w & (1 << bit_index) != 0)
			.count();
		
		let half = (words.len()+1)/2; // ensure we round up, if applicable
		let ord = set_count.cmp(&half);
		println!("common_bit(words_len={}, bi={}) = {:?} than half set ({} total (words.len()/2 => {}))", words.len(), bit_index, ord, set_count, half);
		ord
	}

	/// low bits at `commonalities()[0]`
	/// 
	/// `Ordering::Greater` if more 1s
	fn commonalities(words: &[usize], wordlen: usize) -> Vec<Ordering> {
		(0..wordlen)
			.map(|bi| Day03::common_bit(words, bi))
			.collect()
	}

	fn get_rating(&self, typ: BitCriteria) -> usize {
		let mut candidates = self.words.clone();
	
		for bi in (0..self.wordlen).rev() {
			// get the commonality for this bit
			let ord = Day03::common_bit(&candidates, bi);
			let tgt = typ.should_be_set(ord);

			// explicitly drop before end of scope so length break check works properly
			let _ = candidates.drain_filter(|&mut word| {
				(word & (1 << bi) != 0) != tgt
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

		// from karnaugh map
		// http://www.32x8.com/sop3_____A-B-C_____m_0-3-6-7___________option-0_____999780977071894883684

		let is_equal = matches!(commonality, Equal); // a
		let is_oxy = matches!(self, Oxygen); // b
		let is_greater = matches!(commonality, Greater); // c
		let alt = (is_oxy && (is_greater || is_equal)) || (!is_oxy && !is_equal && !is_greater);

		// derived from above
		let alt2 = match self {
			Oxygen => matches!(commonality, Greater | Equal),
			Co2 => matches!(commonality, Less)
		};

		// reference/"brute force"
		let orig = match (self, commonality) {
			(Oxygen, Equal) => true,
			(Co2, Equal) => false,

			(Oxygen, Greater) => true,
			(Oxygen, Less) => false,

			(Co2, Greater) => false,
			(Co2, Less) => true,
		};

		assert_eq!(orig, alt);
		assert_eq!(orig, alt2);

		orig
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
		let bit_commonality = Day03::commonalities(&self.words, self.wordlen);
		println!("bit_commonality: {:?}", bit_commonality);

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
