#![allow(unused_imports)]
use std::collections::HashMap;
use std::convert::identity;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day09 {
	map: Heightmap,
}
impl Day09 {
}

struct Heightmap {
	/// The heightmap, where each value is an ascii number.
	data: Vec<u8>,
	/// How wide each 'line' is
	width: usize,
}
impl Heightmap {
	fn as_coordinates(&self, i: usize) -> (usize, usize) {
		(i % self.width, i / self.width)
	}
	fn get(&self, x: usize, y: usize) -> u8 {
		self.data[y*self.width + x]
	}
	fn is_low(&self, x: usize, y: usize) -> bool {
		let this = self.get(x, y);

		self.adjacents(x, y).into_iter()
			.filter_map(|opt| opt.map(|(x, y)| self.get(x, y)))
			.all(|b| b > this)
	}
	fn risk_level(&self, x: usize, y: usize) -> u8 {
		// convert each ascii number to a regular 0-9 digit, add one
		self.data[y*self.width + x] - b'0' + 1
	}

	/// Returns an array of coordinates [top, bottom, right, left] that surround this tile
	/// 
	/// Returns None for a coordinate if it would be out of bounds
	fn adjacents(&self, x: usize, y: usize) -> [Option<(usize, usize)>; 4] {
		let left  = (x > 0)                           .then(|| (x-1, y));
		let right = (x < self.width-1)                .then(|| (x+1, y));
		let down  = (y > 0)                           .then(|| (x, y-1));
		let up    = (y < self.data.len()/self.width-1).then(|| (x, y+1));
		[up, down, right, left]
	}

	/// Finds the low point connected to this tile, and it's associated risk value
	/// 
	/// Returns None if the tile is a 9
	fn basin_low_point(&self, x: usize, y: usize) -> Option<(usize, usize)> {
		let mut best = (x, y, self.get(x, y));

		// 9s are not apart of a basin
		if best.2 == b'9' { return None; }

		// get adjacents -> get their score -> compare with best, reloop if a better was found
		while let Some((score, x, y)) = self.adjacents(best.0, best.1)
			.into_iter()
			.flatten()
			.map(|(x, y)| (self.get(x, y), x, y))
			.find(|&(score, _, _)| score < best.2)
		{
			best = (x, y, score);
		}
		Some((best.0, best.1))
	}
}

impl AoCDay for Day09 {
	type Answer = usize;

	fn day() -> u8 { 9 }
	fn name() -> &'static str { "Smoke Basin" }

	fn parse(input: &str) -> DayResult<Self> {
		let mut line_count = 0;
		let lines: String = input.lines()
			.filter_map(aoch::parsing::trimmed)
			.inspect(|_| { line_count += 1; })
			.collect();
		
		let data = lines.into_bytes();
		let width = data.len()/line_count;

		let map = Heightmap { data, width };
		Ok(Day09 { map })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		// all points -> is a low point -> to risk level -> sum all

		let risks = (0..self.map.data.len())
			.map(|i| self.map.as_coordinates(i))
			.filter(|&(x, y)| self.map.is_low(x, y))
			.map(|(x, y)| self.map.risk_level(x, y) as usize)
			.sum();
		
		Ok(risks)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		// each basin is identified by a low point
		let mut basins: HashMap<(usize, usize), usize> = HashMap::default();

		// all points -> get its low point -> count them into the hashmap above
		(0..self.map.data.len())
			.map(|i| self.map.as_coordinates(i))
			.filter_map(|(x, y)| self.map.basin_low_point(x, y))
			.for_each(|low_point| {
				*basins.entry(low_point).or_insert(0) += 1;
			});

		// sum the three largest basin's sizes'
		Ok(basins.values().sorted().rev().take(3).product())
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
2199943210
3987894921
9856789892
8767896789
9899965678
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 15),
		(daystr!("09"), 500),
	];
	test_runner::<Day09, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 1134),
		(daystr!("09"), 970200),
	];
	test_runner::<Day09, _>(DayPart::Part2, &cases);
}
