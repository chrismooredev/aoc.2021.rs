#![allow(unused_imports)]
use std::io::Write;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day15 {
	cavern: Cavern,
}
impl Day15 {

}

struct Cavern {
	width: usize,
	height: usize,
	tiles: Vec<u8>,
}
impl Cavern {
	fn pathfinding(&self) -> usize {
		// succeed on getting to the end
		let success = |n: &usize| -> bool { *n == self.tiles.len()-1 };

		let successors = |n: &usize| {
			let mut results = self.adjacencies(*n);

			results.sort_by_key(|c_opt| {
				c_opt.map(|i| self.tiles[i])
			});

			results.into_iter()
				.flatten()
				.map(|t| (t, self.tiles[t] as usize))
		};

		let (_path, total) = pathfinding::directed::dijkstra::dijkstra::<usize, usize, _, _, _>(&0, successors, success).unwrap();

		total
	}

	fn adjacencies(&self, around: usize) -> [Option<usize>; 4] {
		let (width, height) = (self.width, self.height);
		let top = (around/width < height - 1).then(|| around + width);
		let bot = (around/width > 0).then(|| around - width);
		let rig = (around % width < width - 1).then(|| around + 1);
		let lef = (around % width > 0).then(|| around - 1);

		[
			top, bot, rig, lef,
		]
	}
	
	/// Repeat the current cavern 5 times in each direction, with each step increasing each value by 1
	fn mul5(&self) -> Cavern {
		let mut tiles = Vec::with_capacity(self.tiles.len() * 5*5);

		// The order of these `for` loops are important. If they are
		// re-arranged, then we would have to constantly shuffle memory
		// since we are using a flat array to store these 2D points
		//
		// The way they are here allow us to simply extend the vector
		// and only copy memory in, rather than around.
		//
		// It also allows us to work on this in chunks of `self.width`
		// which is nice
		for oy in 0..5 {
			for iy in 0..self.height {
				for ox in 0..5 {
					let inc = ox + oy;
					
					let src = &self.tiles[self.width*iy..self.width*(iy+1)];
					tiles.extend_from_slice(src);
					let dst_start = tiles.len() - src.len();
					let dst = &mut tiles[dst_start..];
					for b in dst {
						let zero_idx = *b - 1;
						let nb = (zero_idx + inc) % 9; // increase
						*b = nb + 1;
					}
				}
			}
		}

		Cavern {
			width: self.width*5,
			height: self.height*5,
			tiles,
		}
	}
}

impl AoCDay for Day15 {
	type Answer = usize;

	fn day() -> u8 { 15 }
	fn name() -> &'static str { "Chiton" }

	fn parse(input: &str) -> DayResult<Self> {
		let mut lines: usize = 0;
		let mut nums = Vec::new();
		input.lines()
			.filter_map(aoch::parsing::trimmed)
			.for_each(|line| {
				lines += 1;
				nums.extend_from_slice(line.as_bytes());
			});

		for num in nums.iter_mut() {
			*num -= b'0';
		}

		Ok(Day15 { cavern: Cavern {
			width: nums.len()/lines,
			height: lines,
			tiles: nums
		}})
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		Ok(self.cavern.pathfinding())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let big = self.cavern.mul5();
		eprintln!("cavern tiles raw size: {}", std::mem::size_of::<u8>() * big.tiles.len());
		std::thread::sleep(std::time::Duration::from_secs(5));
		Ok(big.pathfinding())
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 40),
		(daystr!("15"), 769),
	];
	test_runner::<Day15, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 315),
		(daystr!("15"), 2963), // not 2970
	];
	test_runner::<Day15, _>(DayPart::Part2, &cases);
}

