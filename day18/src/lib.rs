#![allow(unused_imports)]
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};
use nom::{IResult, Err};
use nom::branch::alt;
use nom::character::complete::digit1;
use nom::combinator::{map, map_res, opt};
use nom::multi::many1;

struct SnailNumPair(SnailNum, SnailNum);
enum SnailNum {
	Literal(usize),
	Pair(Box<[SnailNum; 2]>)
}
impl SnailNum {
	fn parse(s: &str) -> IResult<&str, SnailNum> {
		use nom::sequence::*;
		use nom::character::complete::*;
		use nom::branch::*;
		use nom::combinator::*;
		
		alt((
			map(
				delimited(
					char('['),
					separated_pair(SnailNum::parse, char(','), SnailNum::parse),
					char(']')
				),
				|(first, second)| SnailNum::Pair(Box::new([first, second]))
			),
			map(
				map_res(digit1, usize::from_str),
				|n| SnailNum::Literal(n),
			)
		))(s)
		
	}
	fn parse_line(s: &str) -> SnailNum {
		let (rest, num) = nom::combinator::all_consuming(SnailNum::parse)(s).unwrap();
		assert!(rest.is_empty());
		num
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SnailToken {
	OpenParen,
	CloseParen,
	Number(u32),
}
impl SnailToken {
	fn tokenize_once(s: &str) -> IResult<&str, SnailToken> {
		use nom::character::complete::char;

		alt((
			map(map_res(digit1, u32::from_str), |n| SnailToken::Number(n)),
			map(char('['), |_| SnailToken::OpenParen),
			map(char(']'), |_| SnailToken::CloseParen),
		))(s)
	}
	fn tokenize(s: &str) -> Vec<SnailToken> {
		use nom::character::complete::char;
		
		let (discard, result) = many1(
			alt((
				map(char(','), |_| None),
				map(SnailToken::tokenize_once, |t| Some(t))
			))
		)(s).unwrap();

		assert_eq!("", discard);
		result.into_iter().flatten().collect()
	}
	fn reduce(v: &mut Vec<SnailToken>) {
		fn explode(v: &mut Vec<SnailToken>, at: usize) {
			// program seems to assert that this pair should be number literals
			if let [
				SnailToken::OpenParen,
				SnailToken::Number(left),
				SnailToken::Number(right),
				SnailToken::CloseParen,
			] = v[at..at+4] {
				let repl = [SnailToken::Number(0)];
				let _ = v.splice(at..at+4, repl);

				if let Some(leftmost) = v[..at].iter_mut().rev()
					.filter_map(|t| if let SnailToken::Number(n) = t { Some(n) } else { None })
					.next()
				{
					*leftmost += left;
				} else {
					// no number to the left, `left` gets ignored
				}

				if let Some(rightmost) = v[at+1..].iter_mut()
					.filter_map(|t| if let SnailToken::Number(n) = t { Some(n) } else { None })
					.next()
				{
					*rightmost += right;
				} else {
					// no number to the right, `right` gets ignored
				}
			} else {
				panic!("unexpected pattern at explosion location @ {}: {:?}", at, &v[at..at+4]);
			}
			
		}
		fn split(v: &mut Vec<SnailToken>, at: usize) {
			if let SnailToken::Number(n) = v[at] {
				let repl = [
					SnailToken::OpenParen,
					SnailToken::Number(n/2), // num, div 2, round down
					SnailToken::Number((n+1)/2), // num, div 2, round up
					SnailToken::CloseParen
				];
				let _ = v.splice(at..at+1, repl);
			} else {
				panic!("number was not found at split point")
			}
		}
		
		/// Returns the index of the opening paran for the first pair that is
		/// nested within at least 4 other pairs
		fn find_depth4(v: &[SnailToken]) -> Option<usize> {
			let mut nest_level = 0;
			for (i, e) in v.iter().enumerate() {
				match e {
					SnailToken::OpenParen if nest_level == 4 => {
						// all the data seems to hold this assertion
						assert!(matches!(v[i+1], SnailToken::Number(_)));
						return Some(i);
					}
					SnailToken::OpenParen => nest_level += 1,
					SnailToken::CloseParen => nest_level -= 1,
					SnailToken::Number(_) => { }
				}
			}

			None
		}
		fn find_over10(v: &[SnailToken]) -> Option<usize> {
			v.iter().position(|t| {
				if let SnailToken::Number(n) = *t {
					n >= 10
				} else {
					false
				}
			})
		}

		loop {
			// eprintln!("reducing expression: {:?}", v);
			// find nums nested inside four pairs, explodes
			if let Some(i) = find_depth4(&v) {
				explode(v, i);
				continue;
			}

			// 10 or greater, splits
			if let Some(i) = find_over10(&v) {
				split(v, i);
				continue;
			}

			break;
		}
	}
	fn add(left: &[SnailToken], right: &[SnailToken]) -> Vec<SnailToken> {
		let mut v = Vec::with_capacity(2 + left.len() + right.len());
		v.push(SnailToken::OpenParen);
		v.extend_from_slice(left);
		v.extend_from_slice(right);
		v.push(SnailToken::CloseParen);
		v
	}
	fn magnitude(v: &[SnailToken]) -> usize {
		fn magnitude_recur(v: &[SnailToken]) -> (usize, usize) {
			let mut i = 0;
			match v[i] {
				SnailToken::Number(n) => (1, n as usize),
				SnailToken::OpenParen => {
					let (left_len, left) = magnitude_recur(&v[1..]);
					let (right_len, right) = magnitude_recur(&v[1+left_len..]);
					assert_eq!(v[1+left_len+right_len], SnailToken::CloseParen);
					(2+left_len+right_len, 3*left+2*right)
				},
				SnailToken::CloseParen => panic!("attempt to get magnitude of closing paren"),
			}
		}

		let (len, val) = magnitude_recur(v);
		assert_eq!(v.len(), len);
		val
	}
}

pub struct Day18 {
	/// Easier to operate on tokens than a parsed tree for this case
	tokens: Vec<Vec<SnailToken>>,
}
impl Day18 {
}

impl AoCDay for Day18 {
	type Answer = usize;

	fn day() -> u8 { 18 }
	fn name() -> &'static str { "Snailfish" }

	fn parse(input: &str) -> DayResult<Self> {
		let pairs = aoch::parsing::from_lines_with(input, SnailNum::parse_line);
		let tokens = aoch::parsing::from_lines_with(input, SnailToken::tokenize);
		Ok(Day18 { pairs, tokens })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let mut tokens = self.tokens.clone();
		let mag_sum = tokens.iter_mut()
			.map(|v| { SnailToken::reduce(v); v })
			.reduce(|left, right| {
				let mut sum = SnailToken::add(&left, &right);
				SnailToken::reduce(&mut sum);
				*left = sum;
				left
			})
			.map(|v| SnailToken::magnitude(&v))
			.unwrap();


		Ok(mag_sum)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let mut tokens = self.tokens.clone();
		tokens.iter_mut()
			.for_each(SnailToken::reduce);
		
		let max_mag = tokens.iter()
			.permutations(2)
			.map(|v| {
				let [left, right]: [&Vec<SnailToken>; 2] = v.try_into().unwrap();
				let mut sum = SnailToken::add(&left, &right);
				SnailToken::reduce(&mut sum);
				sum
			})
			.map(|st| SnailToken::magnitude(&st))
			.max().unwrap();

		Ok(max_mag)
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 4140),
		(daystr!("18"), 4132),
	];
	test_runner::<Day18, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 3993),
		(daystr!("18"), 4685),
	];
	test_runner::<Day18, _>(DayPart::Part2, &cases);
}
