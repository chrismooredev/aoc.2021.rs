#![allow(unused_imports)]
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

#[derive(Debug, Default)]
struct DeterministicDie {
	last: usize,
	rolled: usize,
}
impl Iterator for DeterministicDie {
	type Item = usize;
	fn next(&mut self) -> Option<Self::Item> {
		self.rolled += 1;
		self.last += 1;
		if self.last == 101 {
			self.last = 1;
		}
		Some(self.last)
	}
}


// frequencies of dice roll sums for 3, 3-sided die, with replacement.
// face_value = i+3
// frequency_of_six = frequencies[6-3];
// for (i, f) in frequencies.iter().enumerate() {
//     let value = i+3;
// }
const FREQUENCIES: [usize; 7] = [1, 3, 6, 7, 6, 3, 1];

/// A possible state the game could be in - player positions and scores
/// 
/// Once this struct has been constructed, it must not change.
/// 
/// 10^2 * 29^2 = 84100 possible states (though given input, only 24-28k states are found)
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct GameState {
	/// state is a u8 where upper nibble is p2 and lower nibble is p1
	/// Where each player is on the board, zero-indexed
	/// P1 has the lower nibble, P2 has the upper nibble
	positions: u8,

	/// Score of player 1 in range of 0 to 29 (20+3+3+3)
	p1_score: u8,
	/// Score of player 2 in range of 0 to 29 (20+3+3+3)
	p2_score: u8,
}
impl GameState {
	fn initial(p1: usize, p2: usize) -> GameState {
		assert!(1 <= p1 && p1 <= 10);
		assert!(1 <= p2 && p2 <= 10);
		GameState {
			positions: ((p2 as u8-1) << 4) | (p1 as u8-1),
			p1_score: 0,
			p2_score: 0,
		}
	}
	fn p1_pos0(&self) -> u8 { self.positions & 0b1111 }
	fn p2_pos0(&self) -> u8 { self.positions >> 4 }

	fn p1_wins(&self) -> bool { self.p1_score >= 21 }
	fn p2_wins(&self) -> bool { self.p2_score >= 21 }

	fn p1_advance(&self, amt: u8) -> GameState {
		debug_assert!(3 <= amt && amt <= 9, "attempt to advance player 1 out of bounds of three dice roll (attempted {})", amt);
		let mut ns = *self;
		let p1_pos = (ns.p1_pos0() + amt) % 10;
		ns.positions = (ns.p2_pos0() << 4) | p1_pos;
		ns.p1_score += p1_pos+1;
		ns
	}
	fn p2_advance(&self, amt: u8) -> GameState {
		debug_assert!(3 <= amt && amt <= 9, "attempt to advance player 2 out of bounds of three dice roll (attempted {})", amt);
		let mut ns = *self;
		let p2_pos = (ns.p2_pos0() + amt) % 10;
		ns.positions = (p2_pos << 4) | ns.p1_pos0();
		ns.p2_score += p2_pos+1;
		ns
	}

	fn next_game_states(&self) -> GameStatePredictor {
		GameStatePredictor {
			source: *self,
			upper: FREQUENCIES.iter().enumerate(),
			lower: None,
		}
	}
}
impl fmt::Debug for GameState {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("GameState")
			.field("positions", &[self.p1_pos0()+1, self.p2_pos0()+1])
			.field("scores", &[self.p1_score, self.p2_score])
			.finish()
	}
}

#[derive(Debug)]
struct GameStatePredictor {
	source: GameState,
	upper: std::iter::Enumerate<std::slice::Iter<'static, usize>>,
	lower: Option<(GameState, usize, std::iter::Enumerate<std::slice::Iter<'static, usize>>)>,
}
impl Iterator for GameStatePredictor {
	type Item = (GameState, usize);
	fn next(&mut self) -> Option<Self::Item> {
		// eprint!(".");
		match self.lower.as_mut() {
			None => { /* no pending P2 options */ },
			Some((p1_next, f1, iter)) => {
				match iter.next() {
					None => {
						// clear this one out
						self.lower = None
					},
					Some((i2, f2)) => {
						let v2 = (i2+3) as u8;
						let p2_next = p1_next.p2_advance(v2);

						// does this need to be multiplication instead of addition?
						return Some((p2_next, *f1 * *f2));
					}
				}
			}
		};
		if let Some((i1, f1)) = self.upper.next() {
			let v1 = (i1+3) as u8;
			let p1_next = self.source.p1_advance(v1);
			if p1_next.p1_wins() {
				return Some((p1_next, *f1));
			} else {
				self.lower = Some((p1_next, *f1, FREQUENCIES.iter().enumerate()));
				return self.next(); // continue back around
			}
		}

		None
	}
}

pub struct Day21 {
	starting_positions: Vec<usize>,
}
impl Day21 {
}

impl AoCDay for Day21 {
	type Answer = usize;

	fn day() -> u8 { 21 }
	fn name() -> &'static str { "Dirac Dice" }

	fn parse(input: &str) -> DayResult<Self> {
		aoch::parsing::try_from_lines_with(input, |line| {
			let num_str = line.split(' ').last().unwrap();
			num_str.parse()
		})	
			.map(|starting_positions| Day21 { starting_positions })
			.map_err(|e| e.into())
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut die = DeterministicDie::default();
		let mut players = self.starting_positions.clone();
		let mut scores = vec![0; players.len()];

		// put player positions at 0-indexed
		players.iter_mut().for_each(|n| *n -= 1);

		'outer: loop {
			for (_i, (place, score)) in players.iter_mut().zip(scores.iter_mut()).enumerate() {
				// let rolled: Vec<usize> = (&mut die).take(3).collect_vec();
				// let forward: usize = rolled.iter().sum();
				let forward: usize = (&mut die).take(3).sum();
				// eprintln!("player {} (at {}, score {}) rolled {:?} (sum {}, to {})", i+1, *place%10+1, *score, rolled, forward, (*place+forward)%10+1);
				*place += forward;
				*score += (*place % 10) + 1;

				if *place > 1_000_000 { // keep it reasonable
					*place -= 1000;
				}
				if *score >= 1000 { // game over
					break 'outer;
				}
			}
			// eprintln!("player positions, scores: {:?} {:?}", [players[0]%10+1, players[1]%10+1], scores);
		}

		eprintln!("scores: {:?}, die rolled: {}", scores, die.rolled);
		Ok(scores.iter().copied().min().unwrap() * die.rolled)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		
		// <game_state, number_of_universes>
		let mut map: HashMap<GameState, usize> = HashMap::new();

		// seed our initial universe with input data
		map.insert(GameState::initial(self.starting_positions[0], self.starting_positions[1]), 1);

		let mut p1_wins = 0;
		let mut p2_wins = 0;

		// let mut unique_states = HashSet::new();

		for _i in 0.. {
			// eprintln!("starting iteration {}... ({} states)", _i, map.len());
			// if map.len() < 12 {
			// 	for (gs, c) in map.iter().sorted_by_key(|(_gs, c)| *c) {
			// 		eprintln!("\t{:>3} :: {:?}", c, gs);
			// 	}
			// }

			// map.keys().for_each(|gs| { unique_states.insert(*gs); });

			map = map.iter()
				.filter(|(gs, p)| {
					if gs.p1_wins() { p1_wins += **p; return false; }
					if gs.p2_wins() { p2_wins += **p; return false; }
					true
				})
				.map(|(gs, p)| {
					gs.next_game_states()
						.map(move |(ngs, c)| (ngs, p*c))
				})
				.flatten()
				.fold(HashMap::new(), |mut hm, (gs, univ_count)| {
					*hm.entry(gs).or_default() += univ_count;
					hm
				});

			if map.len() == 0 {
				// everyone has won
				break;
			}
		}

		// additional logging to help diagnose issues
		// rwins was from example to test approx magnitude of the answer

		// eprintln!("unique states: {}", unique_states.len());
		// eprintln!("p1_wins:  {:>18}", p1_wins);
		// eprintln!("p1_rwins: {:>18}", 444356092776315usize);
		// eprintln!("p2_wins:  {:>18}", p2_wins);
		// eprintln!("p2_rwins: {:>18}", 341960390180808usize);

		Ok(p1_wins.max(p2_wins))
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
Player 1 starting position: 4
Player 2 starting position: 8
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 739785),
		(daystr!("21"), 913560),
	];
	test_runner::<Day21, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 444356092776315),
		(daystr!("21"), 110271560863819),
	];
	test_runner::<Day21, _>(DayPart::Part2, &cases);
}
