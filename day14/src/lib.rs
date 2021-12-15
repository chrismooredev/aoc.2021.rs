#![feature(int_roundings)]

#![allow(unused_imports)]
use std::fmt;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day14 {
	template: String,
	rules: Vec<([u8; 2], u8)>,
}
impl Day14 {
}

/// Assumes all bytes given are uppercase, ASCII letters
struct PolymerChain(Vec<u8>);
impl PolyChain for PolymerChain {
	fn new(template: &str) -> PolymerChain {
		PolymerChain(template.to_string().into_bytes())
	}
	fn apply_rules(&self, rules: &PolymerRules) -> PolymerChain {
		let mut next = Vec::with_capacity(self.0.len()*2-1);

		for i in 0..self.0.len()-1 {
			// get the character
			let insert_char: u8 = rules.get(self.0[i], self.0[i+1]).unwrap();

			// push both `i` and the mapped character
			next.push(self.0[i]);
			next.push(insert_char + b'A');
		}
		next.push(*self.0.last().unwrap());
 
		PolymerChain(next)
	}
	fn counts(&self) -> [usize; 26] {
		let mut counts = [0; 26];
		for &c in &self.0 {
			// should panic on underflow or bounds if char is not uppercase ascii
			counts[(c - b'A') as usize] += 1;
		}
		counts
	}
	fn len(&self) -> usize { self.0.len() }
}
impl fmt::Debug for PolymerChain {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(std::str::from_utf8(&self.0).unwrap())
	}
}

/// Polymer rules that a chain can follow. Comprised of a list of `AB -> C`
struct PolymerRules {
	/// stores each rule as a populated entry in a 26*26 array
	/// each rule is looked up by index, as if a 2D array with
	/// `26*a + b` where `a` and `b` are both numbers 0-25
	/// representing the letter's index.
	rules: [Option<u8>; 26*26],
}
impl PolymerRules {
	/// Compiles a list of rules comprised of ASCII characters, as bytes.
	/// Each chararacter should be a capitalized letter.
	fn new(raw: &[([u8; 2], u8)]) -> PolymerRules {
		let mut rules = [None; 26*26];

		for &([a, b], out) in raw {
			assert!(a.is_ascii_uppercase());
			assert!(b.is_ascii_uppercase());
			assert!(out.is_ascii_uppercase());
			let a = (a - b'A') as usize;
			let b = (b - b'A') as usize;
			rules[26*a + b] = Some(out - b'A');
		}

		PolymerRules { rules }
	}

	/// Takes ASCII characters and retrieves their rule. The returned byte is of range `[0, 26)`
	fn get(&self, a: u8, b: u8) -> Option<u8> {
		let a = (a - b'A') as usize;
		let b = (b - b'A') as usize;
		self.rules[26*a + b]
	}
}

struct AbstractPolymerChain {
	pairs: [usize; 26*26],
	head: u8,
	tail: u8,
}
impl PolyChain for AbstractPolymerChain {
	fn new(template: &str) -> AbstractPolymerChain {
		let tbytes = template.as_bytes();
		let mut pairs = [0; 26*26];
		for i in 0..tbytes.len()-1 {
			let a = (tbytes[i  ] - b'A') as usize;
			let b = (tbytes[i+1] - b'A') as usize;
			pairs[26*a + b] += 1;
		}
		AbstractPolymerChain {
			pairs,
			head: tbytes.first().unwrap() - b'A',
			tail: tbytes.last().unwrap() - b'A',
		}
	}
	fn apply_rules(&self, rules: &PolymerRules) -> AbstractPolymerChain {
		let opairs = self.pairs;
		let mut npairs = [0; 26*26];

		for i in 0..26*26 {
			let a = i/26;
			let b = i%26;

			let l = opairs[i];
			if l == 0 { continue; }

			let o = match rules.rules[i] {
				Some(b) => b as usize,
				None => panic!("no rule found for {}{}", (a as u8 + b'A') as char, (b as u8 + b'A') as char),
			};

			npairs[26*a + o] += l;
			npairs[26*o + b] += l;
		}

		AbstractPolymerChain { head: self.head, tail: self.tail, pairs: npairs }
	}
	fn counts(&self) -> [usize; 26] {
		let mut counts = [0; 26];

		// separate each pair into individual counts
		for i in 0..self.pairs.len() {
			let l = self.pairs[i];
			let a = i/26;
			let b = i%26;
			counts[a] += l;
			counts[b] += l;
		}

		for i in 0..26 {
			let count = &mut counts[i as usize];
			if *count == 0 { continue; }

			// counting both sides of each pair turns into counting each letter twice
			// remove the duplicated counts
			*count /= 2;

			// above rounded down, add the head/tail sides back in since they would have been a 0.5
			if i == self.head || i == self.tail {
				*count += 1;
			}
		}
		
		counts
	}
	fn len(&self) -> usize {
		self.pairs.iter()
			.copied()
			.sum::<usize>() + 1
	}
}
impl fmt::Debug for AbstractPolymerChain {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		struct Pairs<'a>(&'a [usize; 26*26]);
		impl<'a> fmt::Debug for Pairs<'a> {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				f.debug_map().entries(
					self.0.iter().copied()
					.enumerate()
					.filter(|(_, count)| *count > 0)
					.map(|(i, count)| {
						let a = ((i/26) as u8 + b'A') as char;
						let b = ((i%26) as u8 + b'A') as char;
						([a, b].into_iter().collect::<String>(), count)
					})
				).finish()
			}
		}

		f.debug_struct("AbstractPolymerChain")
			.field("head", &((self.head + b'A') as char))
			.field("tail", &((self.tail + b'A') as char))
			.field("pairs", &Pairs(&self.pairs))
			.finish()
	}
}


trait PolyChain {
	/// Creates a new chain from a capitalized ASCII representation.
	fn new(template: &str) -> Self;
	fn apply_rules(&self, rules: &PolymerRules) -> Self;
	/// Counts occurances of each individual letter within the chain.
	fn counts(&self) -> [usize; 26];
	/// The number of times the highest occuring letter appears, minus the number of appearences of the lowest occuring letter.
	fn score(&self) -> usize {
		let counts = self.counts();
		let (min, max) = counts.iter()
			.filter(|&&c| c > 0)
			.minmax()
			.into_option().unwrap();
		max - min
	}
	/// The length of the chain - how many elements show up.
	fn len(&self) -> usize {
		self.counts().into_iter().sum()
	}
	fn print_counts(&self) {
		self.counts().into_iter()
			.enumerate()
			.filter(|(_, c)| *c > 0)
			.for_each(|(i, c)| eprintln!("\t{}: {}", ((i as u8) + b'A') as u8 as char, c));
	}
}

impl AoCDay for Day14 {
	type Answer = usize;

	fn day() -> u8 { 14 }
	fn name() -> &'static str { "extended polymerization" }

	fn parse(input: &str) -> DayResult<Self> {
		let (template, pairs) = input.split_once("\n\n").unwrap();
		let template = template.trim().to_string();
		let rules: Vec<([u8; 2], u8)> = pairs.lines()
			.filter_map(aoch::parsing::trimmed)
			.map(|l| l.split_once(" -> ").unwrap())
			.map(|(rule, out)| {
				(rule.as_bytes().try_into().unwrap(), out.as_bytes()[0])
			})
			.collect();

		Ok(Day14 { template, rules })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut chain = AbstractPolymerChain::new(&self.template);
		let rules = PolymerRules::new(&self.rules);
		
		for _ in 1..=10 {
			chain = chain.apply_rules(&rules);
		}

		Ok(chain.score())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut chain = AbstractPolymerChain::new(&self.template);
		let rules = PolymerRules::new(&self.rules);

		for _ in 1..=40 {
			chain = chain.apply_rules(&rules);
		}

		Ok(chain.score())
	}
}

/// Ensure the reference and efficient implementations produce consistent results
#[test]
fn chain_counts() {
	let initial = "NNCB";
	const STEPS: [(&str, usize); 11] = [
		// (len, )
		("NNCB", 4), // 0
		("NCNBCHB", 7), // 1
		("NBCCNBBBCBHCB", 13), // 2
		("NBBBCNCCNBBNBNBBCHBHHBCHB", 25), // 3
		("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB", 49), // 4
		("NBBNBBNBBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHB", 97), // 5
		("NBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCB", 193), // 6
		("NBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCHBHHBCHBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHB", 385), // 7
		("NBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCCNBCNCCNBBNBNBBCBHCBHHNHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCB", 769), // 8
		("NBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCHBHHBCHBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCCNBCNCCNBBNBNBBCBHCBHHNHCBBCBHCBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCHBHHBCHBNBBNBBNBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHB", 1537), // 9
		("NBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCCNBCNCCNBBNBNBBCBHCBHHNHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCCNBCNCCNBBNBNBBCBHCBHHNHCBBCBHCBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCHBHHBCHBHHNHCNCHBCHBNBBCHBHHBCHBNBBNBBNBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCNCCNBBBCHBHHBCHBNBBCCNBCNCCNBBNBNBBCBHCBHHNHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCBHCBHHNHCBBCBHCBHHNHCNCHBCCNBCBHCBBCBHCBBNBBNBBCBHCBHHNHCBBCBHCB", 3073), // 10
	];
	
	let rules: PolymerRules = PolymerRules::new(&[
		([b'C', b'H'], b'B'), // CH -> B
		([b'H', b'H'], b'N'), // HH -> N
		([b'C', b'B'], b'H'), // CB -> H
		([b'N', b'H'], b'C'), // NH -> C
		([b'H', b'B'], b'C'), // HB -> C
		([b'H', b'C'], b'B'), // HC -> B
		([b'H', b'N'], b'C'), // HN -> C
		([b'N', b'N'], b'C'), // NN -> C
		([b'B', b'H'], b'H'), // BH -> H
		([b'N', b'C'], b'B'), // NC -> B
		([b'N', b'B'], b'B'), // NB -> B
		([b'B', b'N'], b'B'), // BN -> B
		([b'B', b'B'], b'N'), // BB -> N
		([b'B', b'C'], b'B'), // BC -> B
		([b'C', b'C'], b'N'), // CC -> N
		([b'C', b'N'], b'C'), // CN -> C
	]);

	let mut pc = PolymerChain::new(initial);
	let mut ac = AbstractPolymerChain::new(initial);

	for (i, &(chain, len)) in STEPS.iter().enumerate() {
		let rc = PolymerChain::new(chain);

		assert_eq!(chain.len(), len);
		assert_eq!(rc.len(), pc.len(), "step {}: reference and PolyChain length not equal", i);
		assert_eq!(rc.len(), ac.len(), "step {}: reference and APolyChain length not equal", i);

		assert_eq!(rc.counts(), pc.counts(), "step {}: reference and PolyChain counts not equal", i);
		assert_eq!(rc.counts(), ac.counts(), "step {}: reference and APolyChain counts not equal", i);

		pc = pc.apply_rules(&rules);
		ac = ac.apply_rules(&rules);
	}
}


#[cfg(test)]
const TEST_INPUT: &'static str = "
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 1588),
		(daystr!("14"), 3411),
	];
	test_runner::<Day14, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 2188189693529),
		(daystr!("14"), 7477815755570),
	];
	test_runner::<Day14, _>(DayPart::Part2, &cases);
}
