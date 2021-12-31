#![allow(unused_imports)]
use std::cmp::Ordering;
use std::collections::{HashSet, HashMap};
use std::ops::{Index, IndexMut};
use std::str::FromStr;
use std::time::Instant;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

// I'm not proud of this one. But it does run both parts in under 10ish minutes (albeit with around 8GB ram but still)

#[derive(Debug, Clone, Copy)]
pub enum Reg {
	W,
	X,
	Y,
	Z,
}
impl Reg {
	fn parse(s: &str) -> Reg {
		match s.trim() {
			"w" => Reg::W,
			"x" => Reg::X,
			"y" => Reg::Y,
			"z" => Reg::Z,
			_ => panic!("bad register character: {:?}", s),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Opr {
	Reg(Reg),
	Val(isize),
}
impl Opr {
	fn parse(s: &str) -> Opr {
		match s.trim() {
			"w" | "x" | "y" | "z" => Opr::Reg(Reg::parse(s)),
			_ => Opr::Val(s.parse().unwrap())
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alu([isize; 4]);
impl Alu {
	const ZERO: Alu = Alu([0, 0, 0, 0]);

	pub fn execute(&mut self, instr: Instruction, input: &mut Vec<isize>) {
		match instr {
			Instruction::Inp(a) => self[a] = input.pop().expect("program expected input but received none"),

			Instruction::Add(a, Opr::Reg(b)) => self[a] += self[b],
			Instruction::Mul(a, Opr::Reg(b)) => self[a] *= self[b],
			Instruction::Div(a, Opr::Reg(b)) => { debug_assert_ne!(self[b], 0); self[a] /= self[b] },
			Instruction::Mod(a, Opr::Reg(b)) => { debug_assert_ne!(self[b], 0); self[a] %= self[b] },
			Instruction::Eql(a, Opr::Reg(b)) => self[a] = if self[b] == self[a] { 1 } else { 0 },

			Instruction::Add(a, Opr::Val(b)) => self[a] += b,
			Instruction::Mul(a, Opr::Val(b)) => self[a] *= b,
			Instruction::Div(a, Opr::Val(b)) => { debug_assert_ne!(b, 0); self[a] /= b },
			Instruction::Mod(a, Opr::Val(b)) => { debug_assert_ne!(b, 0); self[a] %= b },
			Instruction::Eql(a, Opr::Val(b)) => self[a] = if b == self[a] { 1 } else { 0 },
		}
	}
}
impl Index<Reg> for Alu {
	type Output = isize;
	fn index(&self, index: Reg) -> &Self::Output {
		let as_ind = index as usize;
        if as_ind >= 4 { unsafe { std::hint::unreachable_unchecked(); } }
		&self.0[as_ind]
	}
}
impl IndexMut<Reg> for Alu {
	fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
		let as_ind = index as usize;
        if as_ind >= 4 { unsafe { std::hint::unreachable_unchecked(); } }
		&mut self.0[as_ind]
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
	Inp(Reg),
	Add(Reg, Opr),
	Mul(Reg, Opr),
	Div(Reg, Opr),
	Mod(Reg, Opr),
	Eql(Reg, Opr),
}
impl Instruction {
	pub fn parse(s: &str) -> Instruction {
		let parts: Vec<&str> = s.trim().split(' ').collect();
		match parts[0] {
			"inp" => Instruction::Inp(Reg::parse(parts[1])),
			"add" => Instruction::Add(Reg::parse(parts[1]), Opr::parse(parts[2])),
			"mul" => Instruction::Mul(Reg::parse(parts[1]), Opr::parse(parts[2])),
			"div" => Instruction::Div(Reg::parse(parts[1]), Opr::parse(parts[2])),
			"mod" => Instruction::Mod(Reg::parse(parts[1]), Opr::parse(parts[2])),
			"eql" => Instruction::Eql(Reg::parse(parts[1]), Opr::parse(parts[2])),
			_ => panic!("unknown instruction: {:?}", parts),
		}
	}
}

pub struct Program<'p>(&'p [Instruction]);
impl<'p> Program<'p> {
	pub fn execute(&self, input: &mut Vec<isize>) -> Alu {
		let mut alu = Alu::ZERO;
		self.execute_with(input, &mut alu);
		alu
	}
	pub fn execute_with(&self, input: &mut Vec<isize>, alu: &mut Alu) {
		for ins in self.0 {
			alu.execute(*ins, input);
		}
	}

	pub fn seperate_groups(&self) -> impl Iterator<Item = [isize; 3]> + 'p {
		use Instruction::*;
		use Reg::*;

		self.0.split(|ins| matches!(ins, Instruction::Inp(Reg::W)))
			.filter(|gr| !gr.is_empty())
			// .inspect(|gr| { eprintln!("group length: {:?}", gr.len()); })
			.enumerate()
			.map(|(i, gr)| {
				if let &[
					Mul(X, Opr::Val(0)),
					Add(X, Opr::Reg(Z)),
					Mod(X, Opr::Val(26)),
					Div(Z, Opr::Val(variable_z)),
					Add(X, Opr::Val(variable_x)),
					Eql(X, Opr::Reg(W)),
					Eql(X, Opr::Val(0)),
					Mul(Y, Opr::Val(0)),
					Add(Y, Opr::Val(25)),
					Mul(Y, Opr::Reg(X)),
					Add(Y, Opr::Val(1)),
					Mul(Z, Opr::Reg(Y)),
					Mul(Y, Opr::Val(0)),
					Add(Y, Opr::Reg(W)),
					Add(Y, Opr::Val(variable_y)),
					Mul(Y, Opr::Reg(X)),
					Add(Z, Opr::Reg(Y))
				] = gr {
					[variable_x, variable_y, variable_z]
				} else {
					todo!("unmatching group #{} - {:?}", i, gr);
				}
			})
			// .inspect(|arr| eprintln!("customs = {:?}", arr))
	}

	pub fn serialize_rs(&self) -> Result<String, std::fmt::Error> {
		use std::fmt::Write;


		let mut s = String::new();
		s.push_str("\nuse std::convert::TryInto;\nuse crate::{Reg, Alu};\n");
		s.push_str("pub fn alu_execute(input: [isize; 14]) -> Option<Alu> {\n");
		s.push_str("\tuse Reg::*;\n");
		s.push_str("\tlet mut alu = Alu::ZERO;\n");


		macro_rules! zero_check {
			(reg $reg: expr) => { write!(s, "\tif alu[{:?}] == 0 {{ return None; }}\n", $reg)? };
			(val $val: expr) => {
				if $val == 0 {
					write!(s, "\treturn None; // div or mul with zero constant\n")?; break;
				}
			};
		}

		s.push_str("\n\t{\n");

		let mut inputs_read = 0;
		for instr in self.0 {
			match instr {
				Instruction::Inp(a) => {
					s.push_str("\t}\n\t\n\t{\n");
					write!(s, "\t\talu[{:?}] = input[{}];\n", a, inputs_read)?; inputs_read += 1;
				},

				Instruction::Add(a, Opr::Reg(b)) => write!(s, "\t\talu[{:?}] += alu[{:?}];\n", a, b)?,
				Instruction::Mul(a, Opr::Reg(b)) => write!(s, "\t\talu[{:?}] *= alu[{:?}];\n", a, b)?,
				Instruction::Div(a, Opr::Reg(b)) => { zero_check!(reg b); write!(s, "\t\talu[{:?}] /= alu[{:?}];\n", a, b)? },
				Instruction::Mod(a, Opr::Reg(b)) => { zero_check!(reg b); write!(s, "\t\talu[{:?}] %= alu[{:?}];\n", a, b)? },
				Instruction::Eql(a, Opr::Reg(b)) => write!(s, "\t\talu[{:?}]  = (alu[{:?}] == alu[{:?}]) as isize;\n", a, b, a)?,

				Instruction::Add(a, Opr::Val(b)) => write!(s, "\t\talu[{:?}] += {};\n", a, b)?,
				Instruction::Mul(a, Opr::Val(b)) => write!(s, "\t\talu[{:?}] *= {};\n", a, b)?,
				Instruction::Div(a, Opr::Val(b)) => { zero_check!(val *b); write!(s, "\t\talu[{:?}] /= {};\n", a, b)? },
				Instruction::Mod(a, Opr::Val(b)) => { zero_check!(val *b); write!(s, "\t\talu[{:?}] %= {};\n", a, b)? },
				Instruction::Eql(a, Opr::Val(b)) => write!(s, "\t\talu[{:?}]  = ({} == alu[{:?}]) as isize;\n", a, b, a)?,
			}
		}
		s.push_str("\n\t}\n\n");

		s.push_str("\tSome(alu)\n}");
		Ok(s)
	}
}

pub struct Day24 {
	prog: Vec<Instruction>,
}
impl Day24 {
	fn go_by_stage(&self, default: u8, direction: Ordering) -> [u8; 14] {
		fn run_program_for(keys: [isize; 3], input: u8, carry: isize) -> isize {
			use Instruction::*;
			use Reg::*;

			let [var_x, var_y, var_z] = keys;
			
			let prog = &[
				// Inp(W),
				Mul(X, Opr::Val(0)),
				Add(X, Opr::Reg(Z)),
				Mod(X, Opr::Val(26)),
				Div(Z, Opr::Val(var_z)),
				Add(X, Opr::Val(var_x)),
				Eql(X, Opr::Reg(W)),
				Eql(X, Opr::Val(0)),
				Mul(Y, Opr::Val(0)),
				Add(Y, Opr::Val(25)),
				Mul(Y, Opr::Reg(X)),
				Add(Y, Opr::Val(1)),
				Mul(Z, Opr::Reg(Y)),
				Mul(Y, Opr::Val(0)),
				Add(Y, Opr::Reg(W)),
				Add(Y, Opr::Val(var_y)),
				Mul(Y, Opr::Reg(X)),
				Add(Z, Opr::Reg(Y))
			];
			let prog = Program(prog);
			let mut alu = Alu([input as isize, 0, 0, carry]);
			prog.execute_with(&mut Vec::new(), &mut alu);
			alu[Z]
		}

		// let mut seed = HashMap::new();
		// seed.insert(0, vec![]);
		
		// carries[i] where i = stage
		// carries[i].get(c) where c is the carry
		//   and the result is a list of the ways it got there
		// let mut carries: Vec<HashMap<isize, Vec<Vec<isize>>>> = vec![seed];

		// HashMap<stage, HashMap<result, HashSet<(source carry, source input)>>>
		let mut stages: HashMap<usize, HashMap<isize, HashSet<(isize, u8)>>> = HashMap::new();
		let mut seed = HashMap::new();
		seed.insert(0, HashSet::new());
		stages.insert(0, seed);

		eprintln!("stages: {:?}", stages);
		let prog = Program(&self.prog);

		for (s, keys) in prog.seperate_groups().enumerate() {
			let pstage = stages.get(&s).unwrap();
			let minmax = pstage.keys().minmax().into_option().unwrap();
			eprintln!("running stage #{} for {} carries... (min/max = {:?})", s, pstage.len(), minmax);
			let mut nstage: HashMap<isize, HashSet<(isize, u8)>> = HashMap::new();
			for inp in 1..=9 {
				for carry in pstage.keys().copied() {
					// eprint!("({:?}, {}, {}) = ...", keys, inp, carry);
					let res = run_program_for(keys, inp, carry);
					// eprintln!("{}", res);
					nstage.entry(res)
						.or_default()
						.insert((carry, inp));
				}
			}

			// eprintln!("done with stage #{}, created {} carries...", s, nstage.len());
			stages.insert(s+1, nstage);
		}

		fn cmp_arr(a: &[u8; 14], b: &[u8; 14]) -> Ordering {
			for (an, bn) in a.iter().zip(b.iter()) {
				let order = an.cmp(bn);
				if order != Ordering::Equal { return order; }
			}
			Ordering::Equal
		}
		fn find_working_ones(direction: Ordering, curr: &mut Vec<(isize, u8)>, min: &mut [u8; 14], map: &HashMap<usize, HashMap<isize, HashSet<(isize, u8)>>>) {
			if curr.len() == 14 {
				// we reached the end - map the input values
				let this: [u8; 14] = curr.iter().map(|(_c, i)| *i).collect::<Vec<_>>().try_into().unwrap();
				if cmp_arr(&this, min) == direction {
					*min = this;
				}
			} else {
				let stagei = 14 - curr.len();
				let stage = map.get(&stagei).unwrap();
				// eprint!("si = {}, slen = {}, curr = {:?}", stagei, stage.len(), curr);
				let target = curr.last().unwrap_or(&(0, 255));
				// eprint!(", target = {:?}", target);
				let sources = stage.get(&target.0).unwrap_or_else(|| {
					panic!("unable to find {} within the results for stage {}, (curr: {:?})", target.0, stagei, curr);
				});
				// eprintln!(", sources.len() = {}", sources.len());
				
				for &v in sources.iter() {
					curr.push(v);
					find_working_ones(direction, curr, min, map);
					let popped = curr.pop();
					assert_eq!(popped, Some(v));
				}
			}
		}

		let s14 = stages.get(&14).unwrap();
		let s14_minmax = s14.keys().minmax().into_option().unwrap();
		eprintln!("stage 14 ({} carries, min/max = {:?}) has a zero: {}", s14.len(), s14_minmax, s14.contains_key(&0));

		let mut working = Vec::new();
		let mut min: [u8; 14] = [default; 14];
		find_working_ones(direction, &mut working, &mut min, &stages);

		eprint!("found: ");
		for n in min.iter().rev() {
			eprint!("{}", n);
		}
		eprintln!("");
		
		min.reverse();
		min
	}
}

impl AoCDay for Day24 {
	type Answer = String;

	fn day() -> u8 { 24 }
	fn name() -> &'static str { "Arithmetic Logic Unit" }

	fn parse(input: &str) -> DayResult<Self> {
		let prog = aoch::parsing::from_lines_with(input, Instruction::parse);
		Ok(Day24 { prog })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let prog = Program(self.prog.as_slice());

		eprintln!("instructions: {}", prog.0.len());
		eprintln!("group count: {}", prog.seperate_groups().count());

		Ok(self.go_by_stage(0, Ordering::Greater).iter().map(|s| s.to_string()).collect())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		Ok(self.go_by_stage(255, Ordering::Less).iter().map(|s| s.to_string()).collect())
	}
}

#[cfg(test)]
#[track_caller]
fn compare_for<F: Fn(Vec<isize>) -> Alu>(prog: &[Instruction], rustic: F, input: &[isize]) {
	let rust_out = rustic(input.to_vec());
	let prog_out = Program(prog).execute(&mut input.to_owned());
	assert_eq!(rust_out, prog_out);
}


#[test]
fn alu_neg() {
	let program = &[
		Instruction::Inp(Reg::X),
		Instruction::Mul(Reg::X, Opr::Val(-1)),
	];

	fn rusty(mut i: Vec<isize>) -> Alu {
		let n = i.pop().unwrap();
		Alu([0, -n, 0, 0])
	}

	compare_for(program, rusty, &[2]);
	compare_for(program, rusty, &[-19]);
	compare_for(program, rusty, &[0]);
}

#[test]
fn alu_cmp() {
	let program = &[
		Instruction::Inp(Reg::Z),
		Instruction::Inp(Reg::X),
		Instruction::Mul(Reg::Z, Opr::Val(3)),
		Instruction::Eql(Reg::Z, Opr::Reg(Reg::X)),
	];

	fn rusty(mut i: Vec<isize>) -> Alu {
		let a = i.pop().unwrap();
		let b = i.pop().unwrap();
		Alu([0, b, 0, if a*3 == b { 1 } else { 0 }])
	}

	compare_for(program, rusty, &[2, 9]);
	compare_for(program, rusty, &[-908, 2]);
	compare_for(program, rusty, &[0, 0]);
	compare_for(program, rusty, &[3, -9]);
}


#[test]
fn part1() {
	let cases = [
		(daystr!("24"), "91897399498995"),
	];
	test_runner::<Day24, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(daystr!("24"), "51121176121391"),
	];
	test_runner::<Day24, _>(DayPart::Part2, &cases);
}
