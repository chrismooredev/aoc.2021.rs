#![allow(unused_imports)]
use std::cell::Cell;
use std::collections::{BTreeMap, HashMap};
use std::str::FromStr;
use ascii::AsciiChar;
use bimap::BiMap;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day12 {
	caverns: CavernPool,
}
impl Day12 {
}

struct CavernPoolBuilder {
	next_lower: u8,
	next_upper: u8,
	cave_map: BiMap<String, u8>,
	caves: [CaveNode; 128],
}
impl CavernPoolBuilder {
	pub fn new() -> CavernPoolBuilder {
		let mut cave_map = BiMap::new();

		// "pre-allocate" 'start' and 'end' tokens so I can hard-code checks to 'a' and 'b'
		cave_map.insert("start".to_owned(), b'a');
		cave_map.insert("end".to_owned(),   b'b');

		let caves = vec![CaveNode { visited: Cell::new(0), connected: Vec::new(), }; 128];

		CavernPoolBuilder {
			next_lower: b'c',
			next_upper: b'A',
			cave_map,
			caves: caves.try_into().unwrap(),
		}
	}
	pub fn insert(&mut self, a: &str, b: &str) {
		let ka = self.get_key(a);
		let kb = self.get_key(b);
		
		self.insert_path(ka, kb);
		self.insert_path(kb, ka);
	}

	/// Finds or creates an index into `self.caves` for the given string key
	fn get_key(&mut self, a: &str) -> u8 {
		match self.cave_map.get_by_left(a).copied() {
			Some(k) => k,
			None => {
				let counter = match a.chars().next() {
					Some(c) if c.is_uppercase() => &mut self.next_upper,
					Some(c) if c.is_lowercase() => &mut self.next_lower,
					_ => panic!("empty string or non-letter found for cave key: {:?}", a),
				};

				let key = *counter;
				*counter += 1;
				if *counter & 0x1F >= 0x1A {
					// strip upper 2 bits
					// compare with Z for each
					panic!("reached upper limit on category for {}case letters", if *counter & 0x20 == 0 { "lower" } else { "upper" });
				}

				self.cave_map.insert(a.to_owned(), key);
				key
			}
		}
	}
	fn insert_path(&mut self, from: u8, to: u8) {
		// skip paths leading to start
		if to == b'a' { return; }

		let cave = &mut self.caves[from as usize].connected;

		if cave.contains(&to) {
			panic!("cave map contains reflexive entry for {}-{}",
				self.cave_map.get_by_right(&from).unwrap(),
				self.cave_map.get_by_right(&to).unwrap(),
			);
		} else {
			cave.push(to);
		}
	}
	pub fn build(self) -> CavernPool {
		CavernPool {
			cave_map: self.cave_map.into_iter()
				.map(|(s, b)| (s.to_owned(), b))
				.collect(),
			caves: self.caves,
		}
	}
}

#[derive(Debug, Clone)]
struct CavernPool {
	cave_map: BiMap<String, u8>,
	// caves: HashMap<Cavern, Vec<Cavern>>,
	caves: [CaveNode; 128],
}
impl CavernPool {
	fn path_string(&self, iter: impl IntoIterator<Item = u8>) -> String {
		iter.into_iter()
			.map(|i| self.cave_map.get_by_right(&i).unwrap().as_str())
			.join(",")
	}
	fn paths_recur<const PART1: bool>(&self, mut second_small_visited: bool, from: u8) -> usize {
		if from == b'b' {
			return 1;
		}

		let cave = &self.caves[from as usize];
		let visited = cave.visited.get();
		// eprintln!("cave={}, visited={}", self.cave_map.get_by_right(&from).unwrap(), visited);

		if from.is_ascii_lowercase() {
			match (PART1, visited) {
				(true, 1) | (false, 2) => return 0,
				(false, 1) if second_small_visited => return 0,
				(false, 1) => second_small_visited = true,
				_ => { },
			}
		}

		cave.visited.set(visited + 1);

		let count = cave.connected.iter()
			.map(|&k| self.paths_recur::<PART1>(second_small_visited, k))
			.sum();

		cave.visited.set(visited);

		count
	}

	pub fn paths_iter<const PART1: bool>(&self) -> usize {
		let mut terminals = 0;
		let mut second_small_visited = false;
		// (cave, old_visited, next_cave)
		let mut stack: Vec<(u8, u8, usize)> = vec![(b'a', false)];

		while let Some((from, old_visited, next_cave)) = stack.pop() {
			if from == b'b' {
				terminals += 1;
				continue;
			}

			let cave = &self.caves[from as usize];
			let mut visited = old_visited;
			if from.is_ascii_lowercase() {
				match (PART1, visited) {
					(true, 1) | (false, 2) => continue,
					(false, 1) if second_small_visited => continue,
					(false, 1) => second_small_visited = true,
					_ => { },
				}
			}

			visited += 1;

		}

	}

	fn paths_iter2(&self, part2: bool) -> usize {
		#[derive(Debug, Clone, Copy)]
		enum State {
			Entering { cave: u8, },
			Iterate { cave: u8, visited: u8, next: usize },
			Closing { cave: u8, visited: u8 },
		}

		let mut terminals = 0;
		let mut twice_visited_active = false;
		let mut stack = vec![ State::Entering { cave: b'a', } ];
		while let Some(top) = stack.pop() {
			match top {
				State::Entering { cave } => {
					if cave == b'b' {
						terminals += 1;
						continue;
					}

					let caven = &self.caves[cave as usize];
					let visited = caven.visited.get();

					
					if cave.is_ascii_lowercase() && visited == 1 && (!part2 || twice_visited_active) {
						continue;
					}
					if cave.is_ascii_lowercase() && visited == 1 && part2 && !twice_visited_active   {
						twice_visited_active = true;
					}
					caven.visited.set(visited + 1);

					stack.push(State::Iterate { cave, visited, next: 0 })
				},
				State::Iterate { cave, visited, next } => {
					match self.caves[cave as usize].connected.get(next) {
						None => stack.push(State::Closing { cave, visited }),
						Some(n) => {
							stack.push(State::Iterate { cave, visited, next: next + 1, });
							stack.push(State::Entering { cave: *n })
						}
					}
				},
				State::Closing { cave, visited } => {
					if cave.is_ascii_lowercase() && visited == 1 && part2 && twice_visited_active {
						assert_eq!(self.caves[cave as usize].visited.get(), 2);
						twice_visited_active = false;
					}
					self.caves[cave as usize].visited.set(visited);
				}
			}
		}

		terminals
	}

	fn paths_iter<const SMALL_CAVE_LIMIT: u8>(&self) -> usize {
		// (cave node, index of next cave.connected to follow)
		let mut stack: Vec<(u8, u8)> = vec![(b'a', 0)];

		// how many times we've reached the end
		let mut terminals = 0;

		// macro to avoid lifetime errors moving `stack` into a closure
		macro_rules! pathstr { () => { self.path_string(stack.iter().map(|&(n, _)| n)) }}

		while let Some(&(this, next_idx)) = stack.last() {
			let cave = &self.caves[this as usize];
			let visited = cave.visited.get();

			eprintln!("[this={}, next_idx={}, visited={}] stack: {}", self.cave_map.get_by_right(&this).unwrap(), next_idx, visited, pathstr!());

			if this == b'b' {
				// reached the end
				terminals += 1;

				eprintln!("[found] {}", pathstr!());

				stack.pop().unwrap();
				continue;
			}

			if this.is_ascii_lowercase() && visited >= SMALL_CAVE_LIMIT {
				// reached a small tunnel without permission
				stack.pop().unwrap();
				continue;
			}

			if next_idx as usize == cave.connected.len() {
				// we are done with this cave's paths, exit and undo visited increment
				cave.visited.set(visited - 1);
				stack.pop().unwrap();
				continue;
			}
			if next_idx == 0 {
				// we are just now entering sub caves - set current as visited
				cave.visited.set(visited + 1);
			}

			// add next cave to the list, restart
			stack.last_mut().unwrap().1 += 1;
			stack.push((cave.connected[next_idx as usize], 0));
		}

		terminals
	}
}

#[derive(Debug, Clone)]
struct CaveNode {
	visited: Cell<u8>,
	connected: Vec<u8>,
}

impl AoCDay for Day12 {
	type Answer = usize;

	fn day() -> u8 { 12 }
	fn name() -> &'static str { "Passage Pathing" }

	fn parse(input: &str) -> DayResult<Self> {
		let mut builder = CavernPoolBuilder::new();
		let paths: Vec<_> = input.lines()
			.filter_map(aoch::parsing::trimmed)
			.map(|s| s.split_once('-').unwrap())
			.collect();

		for (a, b) in paths.iter() {
			builder.insert(a, b);
		}

		let caverns = builder.build();
				
		eprintln!("tunnels:");
		caverns.caves.iter().enumerate()
			.filter(|(_, c)| c.connected.len() > 0)
			.for_each(|(i, c)| {
				let src = caverns.cave_map.get_by_right(&(i as u8)).unwrap();
				let dsts = caverns.path_string(c.connected.iter().copied());
				eprintln!("\t{} -> {}", src, dsts);
			});

		Ok(Day12 { caverns, })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let caves = self.caverns.clone();
		Ok(caves.paths_recur::<true>(false, b'a'))
		// Ok(caves.paths_iter2(false))
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let caves = self.caverns.clone();
		
		// Ok(caves.paths_iter2(true))
		Ok(caves.paths_recur::<false>(false, b'a'))
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
start-A
start-b
A-c
A-b
b-d
A-end
b-end
";

#[cfg(test)]
const TEST_INPUT2: &'static str = "
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
";

#[cfg(test)]
const TEST_INPUT3: &'static str = "
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 10),
		(TEST_INPUT2, 19),
		(TEST_INPUT3, 226),
		(daystr!("12"), 3000),
	];
	test_runner::<Day12, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 36),
		(TEST_INPUT2, 103),
		(TEST_INPUT3, 3509),
		(daystr!("12"), 74222),
	];
	test_runner::<Day12, _>(DayPart::Part2, &cases);
}