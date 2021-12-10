use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::num::ParseIntError;
use std::ops::Index;
use std::str::FromStr;

use aoch::{AoCDay, DayResult};
#[allow(unused_imports)] use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day05 {
	vents: Vec<Vent>,
}
impl Day05 {
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Point { x: u16, y: u16, }
impl Point {
	fn new(x: u16, y: u16) -> Point {
		Point { x, y }
	}
}
impl Index<usize> for Point {
	type Output = u16;
	fn index(&self, ind: usize) -> &Self::Output {
		match ind {
			0 => &self.x,
			1 => &self.y,
			_ => panic!("bad index for Point: {} (expected 0=x, or 1=y)", ind),
		}
	}
}
impl fmt::Debug for Point {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "({}, {})", self.x, self.y)
	}
}


#[derive(Debug, Clone, Copy)]
struct Vent {
	start: Point,
	end: Point,
}
impl Vent {
	fn is_diagonal(&self) -> bool {
		!(self.start.x == self.end.x || self.start.y == self.end.y)
	}
	fn points2(&self) -> Box<dyn Iterator<Item = Point>> {
		use Ordering::*;

		let x_dir = self.start.x.cmp(&self.end.x);
		let y_dir = self.start.y.cmp(&self.end.y);

		let boxed_range = |cmp: Ordering, other_cmp: Ordering, axis: usize| -> Box<dyn Iterator<Item = u16>> {
			match (cmp, other_cmp) {
				(Equal, Equal) => Box::new(self.start[axis]..=self.end[axis]),
				(Less, _) => Box::new(self.start[axis]..=self.end[axis]),
				(Equal, _) => Box::new(std::iter::repeat(self.start[axis])),
				(Greater, _) => Box::new((self.end[axis]..=self.start[axis]).rev()),
			}
		};

		let x = boxed_range(x_dir, y_dir, 0);
		let y = boxed_range(y_dir, x_dir, 1);

		fn mapper((x, y): (u16, u16)) -> Point {
			Point::new(x, y)
		}
		Box::new(x.zip(y).map(mapper))
	}
}


#[derive(Debug, thiserror::Error)]
#[error("error parsing a vent line. Expected syntax: `x1,y1 -> x2,y2` (where x1/y1/x2/y2 are integers)")]
enum ParseVentError {
	BadNumberOfPoints,
	BadPointCoordinates,
	NonIntegerPoints(#[from] ParseIntError),
}
impl FromStr for Vent {
	type Err = ParseVentError;
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut points = s.split(" -> ")
			.map(|s| {
				let mut components = s.trim().split(',');
				let x = components.next().ok_or(ParseVentError::BadPointCoordinates)?.parse()?;
				let y = components.next().ok_or(ParseVentError::BadPointCoordinates)?.parse()?;
				if components.next().is_some() {
					return Err(ParseVentError::BadPointCoordinates);
				}
				Ok(Point { x, y, })
			});
		let start = points.next().ok_or(ParseVentError::BadNumberOfPoints)??;
		let end = points.next().ok_or(ParseVentError::BadNumberOfPoints)??;
		if points.next().is_some() {
			return Err(ParseVentError::BadNumberOfPoints);
		}
		Ok(Vent { start, end })
	}
}

impl AoCDay for Day05 {
	type Answer = usize;

	fn day() -> u8 { 5 }
	fn name() -> &'static str { "Hydrothermal Venture" }

	fn parse(input: &str) -> DayResult<Self> {
		aoch::parsing::from_lines(input)	
			.map(|vents| Day05 { vents })
			.map_err(DayError::boxed)
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut hm: HashMap<Point, usize> = Default::default();
		self.vents.iter().copied()
			.filter(|v| !v.is_diagonal())
			// .inspect(|v| {
			// 	eprintln!("vent: {:?}", v);
			// 	eprintln!("\t{:?}", v.points2().collect::<Vec<_>>());
			// })
			.map(|v| v.points2())
			.flatten()
			.for_each(|point| {
				let ent = hm.entry(point).or_insert(0);
				*ent += 1;
			});

		Ok(hm.values()
			.filter(|&&c| c > 1)
			.count())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut hm: HashMap<Point, usize> = Default::default();
		self.vents.iter().copied()
			// .filter(|v| !v.is_diagonal())
			// .inspect(|v| {
			// 	eprintln!("vent: {:?}", v);
			// 	eprintln!("\t{:?}", v.points2().collect::<Vec<_>>());
			// })
			.map(|v| v.points2())
			.flatten()
			.for_each(|point| {
				let ent = hm.entry(point).or_insert(0);
				*ent += 1;
			});

		Ok(hm.values()
			.filter(|&&c| c > 1)
			.count())
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 5),
		(daystr!("05"), 6710),
	];
	test_runner::<Day05, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 12),
		(daystr!("05"), 20121),
	];
	test_runner::<Day05, _>(DayPart::Part2, &cases);
}
