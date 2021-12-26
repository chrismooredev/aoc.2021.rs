#![allow(unused_imports)]
use std::convert::identity;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

#[derive(Debug, Clone, Copy)]
struct Octopus {
	energy: u8,
	flashed: bool,
}
impl Octopus {
	fn should_flash(&self) -> bool {
		!self.flashed && self.energy > 9
	}
	fn inc(&mut self) {
		self.energy += 1;
	}

	/// Resets a flashed octopi
	/// 
	/// If flashed, returns the old energy value
	fn reset(&mut self) -> Option<u8> {
		assert!(self.flashed == (self.energy > 9), "did not flash octopi with energy > 9");
		
		let energy = self.flashed.then(|| self.energy);

		if self.flashed {
			self.energy = 0;
			self.flashed = false;
		}

		energy
	}
}

#[derive(Debug, Clone, Copy)]
struct Cavern {
	step: usize,
	octopi: [Octopus; 10*10],
}
impl Cavern {
	fn step(&mut self) -> usize {
		self.step += 1;

		// increment all energy levels
		self.octopi.iter_mut().for_each(Octopus::inc);

		// go around flashing octopi if their energy level > 9
		// only flashes each octopi once
		while let Some(i) = (0..self.octopi.len())
			.find(|&i| self.octopi[i].should_flash())
		{
			// increase all adjacent octopi, mark current as flashed
			for ai in Day11::adjacencies(i).into_iter().flatten() {
				self.octopi[ai].inc();
			}
			
			self.octopi[i].flashed = true;
		}

		// count how many flashed, reset their energy to zero if so
		self.octopi.iter_mut()
			.filter_map(Octopus::reset)
			.count()
	}
}

pub struct Day11 {
	cavern: Cavern,
}
impl Day11 {

	// assumes width/height are both 10
	fn adjacencies(around: usize) -> [Option<usize>; 8] {
		Day11::adjacencies_n(around, 10, 10)
	}
	fn adjacencies_n(around: usize, width: usize, height: usize) -> [Option<usize>; 8] {
		let top = (around/width < height - 1).then(|| around + width);
		let bot = (around/width > 0).then(|| around - width);
		let rig = (around % width < width - 1).then(|| around + 1);
		let lef = (around % width > 0).then(|| around - 1);
		let tr = (top.is_some() && rig.is_some()).then(|| around + width + 1);
		let tl = (top.is_some() && lef.is_some()).then(|| around + width - 1);
		let br = (bot.is_some() && rig.is_some()).then(|| around - width + 1);
		let bl = (bot.is_some() && lef.is_some()).then(|| around - width - 1);

		[
			top, bot, rig, lef,
			tr, tl, br, bl,
		]
	}
}

impl AoCDay for Day11 {
	type Answer = usize;

	fn day() -> u8 { 11 }
	fn name() -> &'static str { "Dumbo Octopus" }

	fn parse(input: &str) -> DayResult<Self> {
		let mut lines: usize = 0;
		let nums: Vec<_> = input.lines()
			.filter_map(aoch::parsing::trimmed)
			.inspect(|_| { lines += 1; })
			.map(|line| line.bytes())
			.flatten()
			.map(|b| Octopus { energy: b - b'0', flashed: false })
			.collect();
		
		assert_eq!(lines, 10);
		assert_eq!(nums.len(), 10*10);

		Ok(Day11 { cavern: Cavern { step: 0, octopi: nums.try_into().unwrap() }})
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut cavern = self.cavern;
		let mut flashes = 0;
		for _ in 0..100 {
			flashes += cavern.step();
		}
		Ok(flashes)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut cavern = self.cavern;
		loop {
			let flashed = cavern.step();
			if flashed == cavern.octopi.len() {
				return Ok(cavern.step)
			}
		}
	}
}

#[test]
fn adjacent_indices() {
	let c_ind = |x: usize, y: usize| y*10 + x;
	
	let around_coords = Day11::adjacencies(c_ind(5, 4));
	// top, bottom, right, left
	// top-right, top-left, bottom-right, bottom-left
	assert_eq!(around_coords, [
		Some(c_ind(5, 5)), Some(c_ind(5, 3)), Some(c_ind(6, 4)), Some(c_ind(4, 4)),
		Some(c_ind(6, 5)), Some(c_ind(4, 5)), Some(c_ind(6, 3)), Some(c_ind(4, 3)),
	]);
		
}

#[test]
fn world_steps() {
	let steps = [
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526", // 0
"6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637", // 1
"8807476555
5089087054
8597889608
8485769600
8700908800
6600088989
6800005943
0000007456
9000000876
8700006848", // 2
"0050900866
8500800575
9900000039
9700000041
9935080063
7712300000
7911250009
2211130000
0421125000
0021119000", // 3
	];

	let mut world = Day11::parse(steps[0]).unwrap().cavern;
	for i in 0..steps.len() {
		// serialize world to string
		let mut bytes: Vec<u8> = world.octopi.iter()
			.map(|o| o.energy + b'0')
			.collect();
		for i in (1..10).rev() {
			bytes.insert(i*10, b'\n');
		}
		let as_str = std::str::from_utf8(&bytes).unwrap();

		// eprintln!("[reference]  [world]  step={}", i);
		// let world_lines: Vec<&str> = as_str.lines().collect();
		// let reference_lines: Vec<&str> = steps[i].lines().collect();
		// let worlds_side_by_side: String = world_lines.iter().zip(reference_lines.iter())
		// 	.map(|(world, refer)| format!("{} -> {}\n", refer, world))
		// 	.collect();
		// eprintln!("{}", worlds_side_by_side);
		

		// compare with reference
		assert_eq!(steps[i], as_str, "world at step {} not equal", i);

		// advance for next step
		world.step();
	}
}


#[cfg(test)]
const TEST_INPUT: &'static str = "
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 1656),
		(daystr!("11"), 1719),
	];
	test_runner::<Day11, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 195),
		(daystr!("11"), 232),
	];
	test_runner::<Day11, _>(DayPart::Part2, &cases);
}

