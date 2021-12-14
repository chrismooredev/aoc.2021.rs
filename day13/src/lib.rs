#![feature(drain_filter)]

#![allow(unused_imports)]
use std::collections::HashSet;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

mod chars;
pub use chars::RENDERED_CHARS;

#[derive(Debug, Clone, Copy)]
enum Axis { X, Y, }

pub struct Day13 {
	points: Vec<(usize, usize)>,
	folds: Vec<(Axis, usize)>,
}

struct Paper(Vec<(usize, usize)>);
impl Paper {
	fn new(points: &[(usize, usize)]) -> Paper {
		Paper(Vec::from(points))
	}
	fn fold(&mut self, fold: Axis, at: usize) {

		#[inline(always)] // hope that LLVM can lift out the axis branch from each loop
		fn component<'a, T>(axis: Axis, pair: (T, T)) -> T where T: 'a {
			match axis {
				Axis::X => pair.0,
				Axis::Y => pair.1
			}
		}

		let on_axis = self.0.iter()
			.map(|&coord| component(fold, coord))
			.filter(|&c| at == c)
			.count();
		debug_assert_eq!(0, on_axis, "points along fold axis ({:?}={}), this is illegal", fold, at);

		self.0.iter_mut()
			.map(|(x, y)| component(fold, (x, y)))
			.filter(|&&mut c| c > at)
			.for_each(|c| *c = at - (*c - at));

		self.0.sort_unstable();
		self.0.dedup();
	}
	fn visible(&self) -> usize {
		self.0.len()
	}
	fn render_raw(&self) -> String {
		let points: HashSet<(usize, usize)> = self.0.iter().copied().collect();
		let (width, height) = {
			let mut iter = points.iter().copied();
			let (mut x, mut y) = iter.next().unwrap();
			for (nx, ny) in iter {
				if nx > x { x = nx; }
				if ny > y { y = ny; }
			}

			// add an extra to x for extra newline
			(x+1+1, y+1) // to lens
		};
		let mut target = vec![b'.'; width * height];
		
		// newlines
		for y in 0..height {
			target[width*y + width-1] = b'\n';
		}

		// plot points
		for &(x, y) in points.iter() {
			target[width*y + x] = b'#';
		}

		String::from_utf8(target).unwrap()
	}
	fn split_chars(raw: &str) -> String {
		assert_eq!(6, raw.lines().count(), "raw should be height 6");
		assert_eq!(4*8+7, raw.lines().next().unwrap().len(), "raw should be width {}", 4*8+7);
		assert_eq!(5*8*6, raw.len(), "not all lines are the same size");
		const LINE_LEN: usize = 5*8;

		(0..8)
			.map(|ci| {
				let mut s = String::with_capacity(5*6);
				for line_ind in 0..6 {
					let start = line_ind*LINE_LEN + ci*5;
					s.push_str(&raw[start..start+4]);
					s.push('\n');
				}
				assert_eq!(5*6, s.len());
				s
			})
			.map(|s| RENDERED_CHARS.iter()
				.position(|&rs| rs == s.trim())
				.unwrap_or_else(|| panic!("rendered character was not found, maybe it hasn't been saved yet: {}", s)))
			.map(|rci| (rci as u8 + b'A') as char)
			.collect()
	}
	fn render(&self) -> String {
		Paper::split_chars(&self.render_raw())
	}
}

impl AoCDay for Day13 {
	type Answer = String;

	fn day() -> u8 { 13 }
	fn name() -> &'static str { "Transparent Origami" }

	fn parse(input: &str) -> DayResult<Self> {
		let (coords, folds) = input.split_once("\n\n").unwrap();
		let points: Vec<(usize, usize)> = coords.lines()
			.filter_map(aoch::parsing::trimmed)
			.map(|l| l.split_once(',').unwrap())
			.map(|(x, y)| (x.parse().unwrap(), y.parse().unwrap()))
			.collect();
		
		let folds: Vec<(Axis, usize)> = folds.lines()
			.filter_map(aoch::parsing::trimmed)
			.map(|l| l.split(' ').last().unwrap())
			.map(|l| {
				let (axis, ind) = l.split_once('=').unwrap();
				let ind = ind.parse().unwrap();
				let axis = match axis {
					"x" => Axis::X,
					"y" => Axis::Y,
					_ => panic!("unknown axis: {:?}", axis),
				};
				(axis, ind)
			})
			.collect();

		Ok(Day13 { points, folds })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut paper = Paper::new(&self.points);

		let (axis, at) = self.folds[0];
		paper.fold(axis, at);

		Ok(paper.visible().to_string())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut paper = Paper::new(&self.points);

		for &(axis, at) in &self.folds {
			paper.fold(axis, at);
		}

		Ok(paper.render())
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, "17"),
		(daystr!("13"), "737"),
	];
	test_runner::<Day13, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(daystr!("13"), "ZUJUAFHP"),
	];
	test_runner::<Day13, _>(DayPart::Part2, &cases);
}
