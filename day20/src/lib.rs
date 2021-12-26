#![feature(hash_drain_filter)]

#![allow(unused_imports)]
use std::collections::{HashSet, HashMap};
use std::convert::identity;
use std::fmt;
use std::ops::{RangeInclusive, Range};
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

/*
Opportunities for improvement:
	* use a vector to store the lights
	-> would have to increase by an amount proportional to its height/width each step
	-> would allow for fast light lookups by index
	-> index math, rather than coordinate math (that just gets hashed anyway)
	-> would have to do odd math to map old indicies to new indicies
*/


#[derive(Debug)]
struct InfiniteLight;

type ImageImpl = ImageHashMap;
trait Image: Sized {
	fn new(image: Vec<bool>, width: usize) -> Self;
	fn count_lit(&self) -> Result<usize, InfiniteLight>;
	fn process(&self, algorithm: &[bool; 512]) -> Self;
	fn process_n(&mut self, algorithm: &[bool; 512], iterations: usize) {
		for i in 0..iterations {
			if i % 10 == 0 {
				eprintln!("processing image iter {}, render below: ({:?} elements lit)", i, self.count_lit());
				// eprintln!("{}", self.render());
			}
			*self = self.process(algorithm);
		}
	}
	fn render(&self) -> String;
}

struct ImageHashMap {
	/// The image algorithm could state that empty pixels are lit by default,
	/// but we do not want to store "every pixel" within our map.
	/// 
	/// True if default is lit, false if default is unlit.
	default: bool,

	/// 'True' means that it is the opposite of `default` - not necessarily
	/// that it is lit or unlit.
	/// 
	/// This is a map (instead a set) so we can easily skip already
	/// processed entries.
	image: HashMap<(isize, isize), bool>,

	/// Original (time=0) image dimensions, as an `(width, height)` pair
	/// There are no negative pixels to start, so `x=0..width`, `y=0..height`
	orig_dimensions: (usize, usize),
}
impl Image for ImageHashMap {
	fn new(image: Vec<bool>, width: usize) -> ImageHashMap {
		let default = false;
		let height = image.len()/width;
		let image = image.iter().copied()
			.enumerate()
			.filter(|&(_, b)| b)
			.map(|(i, _)| {
				let x = i % width;
				let y = i / width;
				(x as isize, y as isize)
			})
			.map(|c| (c, true))
			.collect();

		ImageHashMap {
			default,
			image,
			orig_dimensions: (width, height),
		}
	}
	fn count_lit(&self) -> Result<usize, InfiniteLight> {
		if self.default {
			Err(InfiniteLight)
		} else {
			Ok(self.image.iter()
				.filter(|&(_, &b)| self.default ^ b)
				.count())
		}
	}
	fn process(&self, algorithm: &[bool; 512]) -> ImageHashMap {
		assert_eq!(512, algorithm.len());
		let mut new_img: HashMap<(isize, isize), bool> = HashMap::new();
		
		// if last was default lit, new is default all (512-1) lit.
		// otherwise, default none (0) lit.
		let new_def = if self.default { algorithm[511] } else { algorithm[0] };

		// for each active (non-default) coordinate
		self.image.iter()
			.filter(|&(_, active)| *active)
			.map(|(coord, _)| *coord) 
			.for_each(|coord| {
				// take each point within a 3x3 matrix around it
				// (this allows for including edges that have active tiles)
				ImageHashMap::section(coord, 2)
					.for_each(|coord| {
						// and see if /it/ should be active (non-default)
						new_img.entry(coord).or_insert_with(|| {
							let lit = algorithm[self.algo_idx(coord)];
							
							// set as active if not the default
							new_def != lit
						});
					});
			});

		ImageHashMap {
			default: new_def,
			image: new_img,
			orig_dimensions: self.orig_dimensions,
		}
	}
	fn render(&self) -> String {
		let mut buf = String::new();
		let ((min_x, max_x), (min_y, max_y)) = self.bounds(2);
		for y in min_y..=max_y {
			for x in min_x..=max_x {
				let active = self.image.get(&(x, y))
					.copied()
					.unwrap_or(false);

				buf.push(if active == self.default { '.' } else { '#' });
			}
			buf.push('\n');
		}

		buf
	}
}
impl ImageHashMap {
	fn section((x, y): (isize, isize), breadth: usize) -> impl Iterator<Item = (isize, isize)> {
		let breadth = breadth as isize;
		(y-breadth..=y+breadth)
			.map(move |y| (x-breadth..=x+breadth).map(move |x| (x, y)))
			.flatten()
	}

	/// Returns an appropriate algorithm index, mapped from the provided
	/// coordinate. Accounts for the default state
	fn algo_idx(&self, coord: (isize, isize)) -> usize {
		ImageHashMap::section(coord, 1)
			.map(|c| self.image.get(&c))
			.map(|opt| {
				match (self.default, opt) {
					// not active - default
					(def, None) => def,

					// not active - default
					(def, Some(false)) => def,

					// active - opposite default
					(def, Some(true)) => !def,
				}
			})
			.enumerate()
			.fold(0, |mut acc, (i, b)| {
				if b {
					acc |= 1 << (9-1-i);
				}
				acc
			})
	}
	
	/// ((min_x, max_x), (min_y, max_y))
	/// 
	/// If `increased` is provided, the min is subtracted and max is added to
	/// as appropriate.
	fn bounds(&self, increased: usize) -> ((isize, isize), (isize, isize)) {
		let (min_x, max_x) = self.image.keys()
			.map(|(x, _y)| x)
			.copied()
			.minmax()
			.into_option()
			.unwrap();
		let (min_y, max_y) = self.image.keys()
			.map(|(_x, y)| y)
			.copied()
			.minmax()
			.into_option()
			.unwrap();
		
		let increased = increased as isize;
		(
			(min_x - increased, max_x + increased),
			(min_y - increased, max_y + increased)
		)
	}
}
impl fmt::Display for ImageHashMap {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str(&self.render())
	}
}

/// Implementation currently doesn't work - but this should turn out faster than
/// the hashmap based implementation once working.
struct ImageVector {
	height: usize,
	width: usize,
	default: bool,
	image: Vec<bool>,

	/// each step will grow the image - at what x, y is the original at?
	///
	/// top-left is 0,0
	origin_offset: (usize, usize),
}
impl Image for ImageVector {
    fn new(image: Vec<bool>, width: usize) -> Self {
        let height = image.len()/width;
		ImageVector {
			image, height, width,
			default: false,
			origin_offset: (0, 0),
		}
    }

    fn count_lit(&self) -> Result<usize, InfiniteLight> {
        if self.default {
			Err(InfiniteLight)
		} else {
			Ok(self.image.iter()
			.map(|&b| b as usize)
			.sum())
		}
    }

    fn process(&self, algorithm: &[bool; 512]) -> Self {
        let mut new = ImageVector::new(vec![false; (self.width+4)*(self.height+4)], self.width+4);

		new.origin_offset = (self.origin_offset.0+2, self.origin_offset.1+2);
		new.default = if self.default {
			algorithm[512-1]
		} else {
			algorithm[0]
		};

		for i in 0..new.image.len() {
			new.image[i] = algorithm[self.sample_old_img_with_new_coord(i)];
		}

		new
    }

    fn render(&self) -> String {
        let mut s = String::with_capacity(self.image.len() + self.height);
		for y in 0..self.height {
			for &lit in &self.image[self.width*y..self.width*(y+1)] {
				s.push(if lit { '#' } else { '.' });
			}
			s.push('\n');
		}
		s
    }
}
impl ImageVector {
	/// around: coordinate index is relative to new image
	fn sample_old_img_with_new_coord(&self, big_around: usize) -> usize {
		fn big2smol(i: usize, width: usize) -> Option<usize> {
			let origx = i % (width+4);
			let origy = i / (width+4);
			let newx = origx.checked_sub(2)?;
			let newy = origy.checked_sub(2)?;
			Some(newy*width + newx)
		}
		fn _smol2big(i: usize, width: usize) -> usize {
			let origx = i % width;
			let origy = i / width;
			let newx = origx + 2;
			let newy = origy + 2;
			newy*(width+4) + newx
		}

		ImageVector::adjacencies_n(big_around, self.width+4, self.height+4)
			.into_iter()
			.rev().enumerate()
			.map(|(i, bind_opt)| {
				// if not in the big one, then is it in the small one?
				let sind_opt = bind_opt.and_then(|bi| big2smol(bi, self.width));
				(i, sind_opt)
			})
			.fold(0, |mut acc, (i, sind_opt)| {
				// get the pixel data, or use the default
				let is_lit = sind_opt
					.map(|i| self.image[i])
					.unwrap_or(self.default);
				if is_lit {
					acc |= 1 << i;
				}
				acc
			})
	}
	fn adjacencies_n(around: usize, width: usize, height: usize) -> [Option<usize>; 9] {
		let top = (around/width < height - 1).then(|| around + width);
		let bot = (around/width > 0).then(|| around - width);
		let rig = (around % width < width - 1).then(|| around + 1);
		let lef = (around % width > 0).then(|| around - 1);
		let tr = (top.is_some() && rig.is_some()).then(|| around + width + 1);
		let tl = (top.is_some() && lef.is_some()).then(|| around + width - 1);
		let br = (bot.is_some() && rig.is_some()).then(|| around - width + 1);
		let bl = (bot.is_some() && lef.is_some()).then(|| around - width - 1);

		[
			tl, top, tr,
			lef, Some(around), rig,
			bl, bot, br,
		]
	}
}


pub struct Day20 {
	algorithm: [bool; 512],
	image: Vec<bool>,
	image_width: usize,
}
impl Day20 {
}

impl AoCDay for Day20 {
	type Answer = usize;

	fn day() -> u8 { 20 }
	fn name() -> &'static str { "Trench Map" }

	fn parse(input: &str) -> DayResult<Self> {
		let mut lines = input.lines()
			.map(str::trim)
			.skip_while(|s| s.is_empty())
			.collect_vec();
		
		let empty = lines.iter().position(|s| s.is_empty())
			.expect("input should have empty separating line");
		
		// eprintln!("lines: {:?}", lines);
		lines.remove(empty);
		let (algo, img) = lines.split_at(empty);
		let algo_vec = algo.iter().copied()
			.map(str::chars)
			.flatten()
			.map(|c| match c {
				'.' => false,
				'#' => true,
				_ => panic!("unexpected character in algorithm: {:?}", c),
			})
			.collect_vec();
		let algo_len = algo_vec.len();
		let algorithm = algo_vec.try_into().unwrap_or_else(|_| {
			panic!("algorithm should be 512 chars long, got {} chars", algo_len)
		});

		assert!(!img.is_empty(), "image has no data");
		let image_width = img[0].len();
		let image = img.iter().copied()
			.inspect(|s| assert_eq!(image_width, s.len(), "image does not have constant length"))
			.map(str::chars)
			.flatten()
			.map(|c| match c {
				'.' => false,
				'#' => true,
				_ => panic!("unexpected character in image: {:?}", c),
			})
			.collect_vec();

		Ok(Day20 { algorithm, image, image_width })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut image = ImageImpl::new(self.image.clone(), self.image_width);
		image.process_n(&self.algorithm, 2);
		Ok(image.count_lit().unwrap())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut image = ImageImpl::new(self.image.clone(), self.image_width);
		image.process_n(&self.algorithm, 50);
		let lit = image.count_lit().unwrap();
		// let cached = image.image.len();
		// let ((minx, maxx), (miny, maxy)) = image.bounds(0);
		// eprintln!("lit: {}, cached: {}, within bounds: {}", lit, cached, (maxx-minx)*(maxy-miny));
		Ok(lit)
	}
}

/*
#[test]
fn fuel_calc() {
	let cases = [
		(100756, 33583),
	];
	run_test(|n| DayMe::calc_fuel(*n), &cases);
}
*/

#[cfg(test)]
const TEST_INPUT: &'static str = "
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
";

#[test]
fn algo_idx() {
	let d20 = Day20::parse(TEST_INPUT).unwrap();
	let img = ImageHashMap::new(d20.image, d20.image_width);
	let loc = (2, 2);
	let idx = img.algo_idx(loc);
	eprintln!("section_coords: {:?}", ImageHashMap::section(loc, 1).collect_vec());
	eprintln!("section_data: {:?}", ImageHashMap::section(loc, 1)
		.map(|c| img.image.get(&c).map(|c| c).unwrap_or(&img.default))
		.collect_vec()
	);
	assert_eq!(34, idx, "got algo index {} at {:?} -- expected {}", idx, loc, 34);
}

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 35),
		(daystr!("20"), 5218),
	];
	test_runner::<Day20, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 3351),
		(daystr!("20"), 15527),
	];
	test_runner::<Day20, _>(DayPart::Part2, &cases);
}
