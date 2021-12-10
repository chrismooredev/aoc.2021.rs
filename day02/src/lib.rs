use std::num::ParseIntError;
use std::str::FromStr;
use thiserror::Error;

use aoch::{AoCDay, DayResult};
#[allow(unused_imports)] use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

pub struct Day02 {
	cmds: Vec<Command>,
}
impl Day02 {
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Position {
	horiz: isize,
	depth: isize,
	aim: isize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Command(Direction, isize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
	Forward,
	Down,
	Up,
}


#[derive(Error, Debug)]
#[error("Invalid direction provided: {0}")]
struct BadDirection(String);

#[derive(Error, Debug)]
#[error("bad command")]
enum BadCommand {
	ElementCount,
	Direction(#[from] BadDirection),
	Number(#[from] ParseIntError),
}


impl FromStr for Command {
    type Err = BadCommand;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split(' ').collect::<Vec<_>>().as_slice() {
			&[dir, num] => {
				Ok(Command(
					dir.parse()?,
					num.parse()?,
				))
			},
			_ => Err(BadCommand::ElementCount),
		}
    }
}
impl FromStr for Direction {
	type Err = BadDirection;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
			"forward" => Ok(Direction::Forward),
			"down" => Ok(Direction::Down),
			"up" => Ok(Direction::Up),
			_ => Err(BadDirection(s.to_owned()))
		}
    }
}

impl AoCDay for Day02 {
	type Answer = isize;

	fn day() -> u8 { 2 }
	fn name() -> &'static str { "Dive!" }

	fn parse(input: &str) -> DayResult<Self> {
		aoch::parsing::from_lines(input)	
			.map(|cmds| Day02 { cmds })
			.map_err(|e| DayError::Wrapped(Box::new(e)))
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		
		let mut pos = Position::default();
		for cmd in &self.cmds {
			match cmd.0 {
				Direction::Forward => pos.horiz += cmd.1,
				Direction::Down => pos.depth += cmd.1,
				Direction::Up => pos.depth -= cmd.1,
			}
		}

		Ok(pos.horiz * pos.depth)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut pos = Position::default();
		for cmd in &self.cmds {
			match cmd.0 {
				Direction::Forward => {
					pos.horiz += cmd.1;
					pos.depth += pos.aim * cmd.1;
				},
				Direction::Down => pos.aim += cmd.1,
				Direction::Up => pos.aim -= cmd.1,
			}
		}

		Ok(pos.horiz * pos.depth)
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
forward 5
down 5
forward 8
up 3
down 8
forward 2
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 150),
		(daystr!("02"), 1451208),
	];
	test_runner::<Day02, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 900),
		(daystr!("02"), 1620141160),
	];
	test_runner::<Day02, _>(DayPart::Part2, &cases);
}
