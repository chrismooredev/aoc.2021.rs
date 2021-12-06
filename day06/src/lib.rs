use std::num::ParseIntError;

use aoch::{AoCDay, DayResult};
#[allow(unused_imports)] use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day06 {
	initial: Vec<u8>,
}
impl Day06 {
}

/// Two impls of the below trait, implemented either with sorted fish, or grouped fish
trait FishField: std::fmt::Debug {
	fn new(day: usize, data: &[u8]) -> Self;
	fn step(&mut self);
	fn len(&self) -> usize;
	fn day(&self) -> usize;
}
type DayImplField = FieldRepr;

#[derive(Debug, Clone)]
struct FieldRepr {
	day: usize,

	// fish[life] = count
	fish: [usize; 9],
}
impl FishField for FieldRepr {
	fn new(day: usize, v: &[u8]) -> FieldRepr {
		let mut fish = [0; 9];
		for f in v.iter().map(|&n| n as usize) {
			assert!(f < fish.len());
			fish[f] += 1;
		}

		FieldRepr { day, fish, }
	}
    fn step(&mut self) {

		let to_spawn = self.fish[0];
		self.fish.copy_within(1.., 0); // 'decrement' all numbers
		self.fish[6] += to_spawn; // 0's reset to 6's
		self.fish[8] = to_spawn; // 0's spawn 8's

		self.day += 1;
    }

    fn len(&self) -> usize {
    	self.fish.iter().sum()
    }
	fn day(&self) -> usize { self.day }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FieldNaive {
	day: usize,
	fish: Vec<u8>,
}
// express field as octal numbers?
impl FishField for FieldNaive {
	fn new(day: usize, fish: &[u8]) -> FieldNaive {
		FieldNaive { day, fish: fish.to_vec() }
	}
	fn len(&self) -> usize { self.fish.len() }
	fn step(&mut self) {
		let mut to_add = 0;
		for n in &mut self.fish {
			if *n == 0 {
				*n = 6+1;
				to_add += 1;
			}
			*n -= 1;
		}
		self.fish.resize(self.fish.len() + to_add, 8);
		self.day += 1;
	}
	fn day(&self) -> usize { self.day }
}

impl AoCDay for Day06 {
	type Answer = usize;

	fn day() -> u8 { 06 }
	fn name() -> &'static str { "Lanternfish" }

	fn parse(input: &str) -> DayResult<Self> {
		let initial: Vec<_> = input.trim()
			.split(',')
			.map(str::parse)
			.collect::<Result<_, ParseIntError>>()?;
		
		Ok(Day06 { initial })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut f = DayImplField::new(0, &self.initial);

		for _ in 0..80 {
			f.step();
		}

		Ok(f.len())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut f = DayImplField::new(0, &self.initial);

		for _ in 0..256 {
			f.step();
		}

		Ok(f.len())
	}
}


#[test]
fn iter_fish_fields() {
	let cases: &[(usize, &[u8])] = &[
		( 0, &[3,4,3,1,2]),
		( 1, &[2,3,2,0,1]),
		( 2, &[1,2,1,6,0,8]),
		( 3, &[0,1,0,5,6,7,8]),
		( 4, &[6,0,6,4,5,6,7,8,8]),
		( 5, &[5,6,5,3,4,5,6,7,7,8]),
		( 6, &[4,5,4,2,3,4,5,6,6,7]),
		( 7, &[3,4,3,1,2,3,4,5,5,6]),
		( 8, &[2,3,2,0,1,2,3,4,4,5]),
		( 9, &[1,2,1,6,0,1,2,3,3,4,8]),
		(10, &[0,1,0,5,6,0,1,2,2,3,7,8]),
		(11, &[6,0,6,4,5,6,0,1,1,2,6,7,8,8,8]),
		(12, &[5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8]),
		(13, &[4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8]),
		(14, &[3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8]),
		(15, &[2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7]),
		(16, &[1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8]),
		(17, &[0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8]),
		(18, &[6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8]),
	];

	fn test_impl<F: FishField>(cases: &[(usize, &[u8])]) {
		let mut curr = F::new(0, &cases[0].1);

		for (d, fish) in cases {
			assert_eq!(curr.day(), *d, "expected day {}, got day {}", *d, curr.day());
			let expected = F::new(*d, fish);
			assert_eq!(curr.len(), expected.len(), "for day {}, expected fish {:?}, got fish {:?}", curr.day(), expected, curr);
			curr.step();
		}
	}

	test_impl::<FieldNaive>(&cases);
	test_impl::<FieldRepr>(&cases);
}


#[cfg(test)]
const TEST_INPUT: &'static str = "3,4,3,1,2";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 5934),
		(daystr!("06"), 352872),
	];
	test_runner::<Day06, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 26984457539),
		(daystr!("06"), 1604361182149),
	];
	test_runner::<Day06, _>(DayPart::Part2, &cases);
}
