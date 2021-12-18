#![allow(unused_imports)]
use std::convert::identity;
use std::fmt;
use std::ops::RangeInclusive;
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};
use regex::{Regex, Match};

#[derive(Debug, Clone, Copy)]
struct Point(isize, isize);

#[derive(Debug, Clone, Copy)]
struct Probe {
	pos: Point,
	velocity: Point,
	// curve_x: Curve,
	// curve_y: Curve,
}
impl Probe {
	fn new(velocity: Point) -> Probe {
		Probe {
			pos: Point(0, 0),
			velocity,
			// curve_x: Curve::new(velocity.0),
			// curve_y: Curve::new(velocity.1),
		}
	}
	fn iter_to(&self, target: &Target) -> ProbeIter {
		ProbeIter {
			next: *self,
			target: target.clone()
		}
	}
	fn step(&self) -> Probe {
		let mut copy = *self;
		copy.pos.0 += copy.velocity.0;
		copy.pos.1 += copy.velocity.1;

		// normalize x towards zero by one each step
		// if velocity is already zero, keep it there.
		if copy.velocity.0 != 0 {
			if copy.pos.0 > 0 {
				copy.velocity.0 -= 1;
			} else if copy.pos.0 < 0 {
				copy.velocity.0 += 1;
			}
		}
	

		// gravity
		copy.velocity.1 -= 1;

		copy
	}
	fn reaches_target(&self, target: &Target) -> bool {
		self.iter_to(target)
			.any(|p| {
				target.0.contains(&p.0) && target.1.contains(&p.1)
			})
	}
	fn on_step_x(step: isize, velo: isize) -> isize {
		let modif = (velo-1)*2 + 5;
		let offset = velo + 1;
		// let leng = velo*2 + 2;
		((-step.pow(2) + modif*step) / 2) - offset
	}
}

/// Can be used for both horizontal and vertical curves
#[derive(Clone, Copy)]
struct Curve {
	velo: isize,
	x_modif: isize,
	y_modif: isize,
	offse: isize,
	opp_sign: isize,
	len: usize,
	y_func: fn(&Curve, usize) -> isize,
}
impl Curve {
	fn new(velo: isize) -> Curve {
		let avelo = velo.abs();
		let modif = (avelo-1)*2 + 5;
		let offse = avelo + 1;
		let len = avelo*2 + 2;
		let sign = velo.signum();

		Curve {
			velo,
			x_modif: sign*modif,
			y_modif: (-velo - 1)*2 + 1,
			offse: sign*offse,
			opp_sign: -sign,
			len: len as usize,
			y_func: if velo < 0 { Curve::step_y_neg } else { Curve::step_y_pos },
		}
	}
	fn step_y(&self, n: usize) -> isize {
		(self.y_func)(self, n)
	}
	fn step_x(&self, n: usize) -> isize {
		let n = n as isize + 1;
		// similar to equations of form ax^2 + bx + c
		let v = ((self.opp_sign*n*n + self.x_modif*n) / 2) - self.offse;

		if v.signum() * self.velo.signum() == -1 {
			// we switched to the other side of x=0
			// clamp to zero
			0
		} else {
			v
		}
	}

	fn hits_y(&self, target: &Target) -> Option<usize> {
		// bisect until we hit a y-value within the target
		assert!(self.step_y(10000) < *target.1.end());
		let target = target.1.clone();
		let target_upper = *target.start();
		let target_range = target.end() - target.start();

		let mut guess: usize = 5000;
		let mut gstep: usize = 0;
		loop {
			// perform bisection
			let res = self.step_y(guess);
			if res > target_upper {

			} else {

			}
		}

		// let mut range = 0..10000;
		// loop {
		// 	let mid = range.start + (range.end - range.start)/2;
		// 	let (begin, end) = (range.start..mid, mid..range.end);
		// 	if 
		// }

		todo!()
	}

	fn step_y_pos(&self, n: usize) -> isize {
		let n = n as isize + 1;
		// similar to equations of form ax^2 + bx + c
		((self.opp_sign*n*n + self.x_modif*n) / 2) - self.offse
	}
	fn step_y_neg(&self, n: usize) -> isize {
		let n = n as isize;
		-n*(n+self.y_modif)/2
	}

	/// Returns the step closest to the provided x value
	fn closest_x(&self, x: isize) -> f64 {
		// thank you wolfram alpha for deriving this monstrosity for me
		let mut sqrt_arg = 8*self.opp_sign*(self.offse+x)+self.x_modif.pow(2);
		eprintln!("sqrt_arg: {}", sqrt_arg);
		// if sqrt_arg < 0 { sqrt_arg *= -1; }
		let sqrt = f64::sqrt(sqrt_arg as f64);
		(sqrt - (2*self.opp_sign) as f64 - (self.x_modif as f64)) / ((2*self.opp_sign) as f64)
	}
}
impl fmt::Debug for Curve {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("Curve")
			.field("velo", &self.velo)
			.field("x_modif", &self.x_modif)
			.field("y_modif", &self.y_modif)
			.field("offse", &self.offse)
			.field("opp_sign", &self.opp_sign)
			.field("len", &self.len)
			.finish()
	}
}

#[test]
fn dump_closest_Xs() {
	let cx: Curve = Curve::new(7);
	let cxv = (20..=30)
		.map(|x| (x, cx.closest_x(x)))
		.map(|(x, s)| format!("({}, {} -> {})", x, s, cx.step_x(s as usize)))
		.collect_vec();
	
	// let cx_min = cx.closest_x(20);
	// let cx_max = cx.closest_x(30);
	panic!("closest_x [min, ..., max] = ({:#?})", cxv);
}

#[test]
fn horiz_curve() {
	// (velocity, x-values at ith step)
	let steps: &[(isize, &[isize])] = &[
		(-8, &[0, -8, -15, -21, -26, -30, -33, -35, -36, -36, -35, -33, -30, -26, -21, -15, -8, 0]),
		(-5, &[0, -5, -9, -12, -14, -15, -15, -14, -12, -9, -5, 0]),
		(-2, &[0, -2, -3, -3, -2, 0]),
		(1, &[0, 1, 1, 0]),
		(4, &[0, 4, 7, 9, 10, 10, 9, 7, 4, 0]),
		(7, &[0, 7, 13, 18, 22, 25, 27, 28, 28, 27, 25, 22, 18, 13, 7, 0]),
	];
	for (velo, values) in steps.iter() {
		let curve = Curve::new(*velo);
		// eprintln!("[x_velo={}] curve = {:?}", velo, curve);
		assert_eq!(values.len(), curve.len);
		(0..curve.len)
			.map(|s| (s, curve.step_x(s)))
			.for_each(|(s, val)| {
				eprintln!("[x_velo={}, step={}] = {} (expected {})", velo, s, val, values[s]);
				assert_eq!(values[s], val, "velocity {}, step {} did not produce expected value {} (got {}) (curve: {:?})", velo, s, values[s], val, curve);
			});
	}
}

#[test]
fn vert_curve() {
	// (velocity, y-values at ith step)
	let steps: &[(isize, &[isize])] = &[
		(-8, &[0, -8, -17, -27, -38, -50, -63, -77, -92, -108, -125, -143, -162, -182, -203, -225, -248, -272, -297, -323]),
		(-5, &[0, -5, -11, -18, -26, -35, -45, -56, -68, -81, -95, -110, -126, -143, -161, -180, -200, -221, -243, -266]),
		(-2, &[0, -2, -5, -9, -14, -20, -27, -35, -44, -54, -65, -77, -90, -104, -119, -135, -152, -170, -189, -209]),
		(1,  &[0, 1, 1, 0, -2, -5, -9, -14, -20, -27, -35, -44, -54, -65, -77, -90, -104, -119, -135, -152]),
		(4,  &[0, 4, 7, 9, 10, 10, 9, 7, 4, 0, -5, -11, -18, -26, -35, -45, -56, -68, -81, -95]),
		(7,  &[0, 7, 13, 18, 22, 25, 27, 28, 28, 27, 25, 22, 18, 13, 7, 0, -8, -17, -27, -38]),
	];
	for (velo, values) in steps.iter() {
		let curve = Curve::new(*velo);
		// eprintln!("[y_velo={}] curve = {:?}", velo, curve);
		(0..values.len())
			.map(|s| (s, curve.step_y(s)))
			.for_each(|(s, val)| {
				eprintln!("[y_velo={}, step={}] = {} (expected {})", velo, s, val, values[s]);
				assert_eq!(values[s], val, "velocity {}, step {} did not produce expected value {} (got {}) (curve: {:?})", velo, s, values[s], val, curve);
			});
	}
}

/*
len = x*2 + 2
mod = (x-1)*2 + 5
end = x - 1
10 (len 22) :: (23, 11) -1/2*x^2 + (23/2)*x - 11
11 (len 24) :: (25, 12) :: -1/2*x^2 + (25/2)*x - 12
12 (len 26) :: (27, 13) :: -1/2*x^2 + (27/2)*x - 13

def curve_x_eq(velo):
  avelo = abs(velo)
  mod = (avelo-1)*2 + 5
  off = avelo + 1
  len = avelo*2 + 2
  sign = 1 if velo >= 0 else -1
  
  print(f"velo = {velo}, mod = {mod}, off = {off}, len = {len}, sign = {sign}")

  eq = lambda x: ((-sign*x**2 + sign*mod*x) // 2) - sign*off
  for x in range(1, len+1):
    yield eq(x)

list(curve_x_eq(-10))
list(curve_x(-10))
[x_curves_eq(n) for n in range(-50, 50) if n != 0]

-8: n*(n+15)/2
-7: n*(n+13)/2
-6: n*(n+11)/2
-5: n*(n+9)/2
-4: n*(n+7)/2
-3: n*(n+5)/2
-2: n*(n+3)/2
-1: n*(n+1)/2

def curve_y(velo):
  ovelo = velo
  y = 0
  while True:
    yield y
    y += velo
    velo -= 1

list(itertools.islice(curve_y(10), 20))

def curve_x(velo):
  ovelo = velo
  x = 0
  while True:
    yield x
    if x == 0 and ovelo != velo: return
    x += velo
    if x > 0: velo -= 1
    elif x < 0: velo += 1

list(itertools.islice(curve_x(4), 10))

l = list(curvex(3))
l
*/

#[derive(Debug, Clone)]
struct ProbeIter {
	next: Probe,
	target: Target,
}
impl Iterator for ProbeIter {
	type Item = Point;
	fn next(&mut self) -> Option<Self::Item> {
		let below_target = self.next.pos.1 < *self.target.1.start();
		let going_down = self.next.velocity.1 <= 0;
		if below_target && going_down {
			// will never reach target
			None
		} else {
			let to_yield = self.next.pos;
			self.next = self.next.step();
			Some(to_yield)
		}
	}
}

#[derive(Debug, Clone)]
struct Target(RangeInclusive<isize>, RangeInclusive<isize>);
impl Target {
	fn from_raw(points: [isize; 4]) -> Target {
		Target(
			RangeInclusive::new(points[0].min(points[1]), points[0].max(points[1])),
			RangeInclusive::new(points[2].min(points[3]), points[2].max(points[3])),
		)
	}
}

pub struct Day17 {
	target: Target,
}
impl Day17 {
}

impl AoCDay for Day17 {
	type Answer = isize;

	fn day() -> u8 { 17 }
	fn name() -> &'static str { "Trick Shot" }

	fn parse(input: &str) -> DayResult<Self> {
		let re = Regex::new(r"^target area: x=([-0-9]+)..([-0-9]+), y=([-0-9]+)..([-0-9]+)$").unwrap();
		let cap = re.captures(input.trim()).unwrap();
		eprintln!("caps: {:?}", cap);
		let caps = cap.iter()
			.skip(1)
			.map(Option::unwrap)
			.map(|m| m.as_str().parse())
			.map(Result::unwrap)
			.collect::<Vec<isize>>();
		let bounds: [isize; 4] = caps.try_into().unwrap();

		Ok(Day17 {
			target: Target::from_raw(bounds)
		})
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		// two checks:

		let highest_y = (0..1000)
			.cartesian_product(0..1000)
			.map(|(vx, vy)| Probe::new(Point(vx, vy)))
			.filter(|p| p.reaches_target(&self.target))
			.map(|p| p.iter_to(&self.target))
			.flatten()
			.map(|Point(_, y)| y)
			.max().expect("found no valid trajectories");

		Ok(highest_y)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let lower = *self.target.1.start();
		let valid = (0..1000)
			.cartesian_product(lower..=lower+1000)
			.map(|(vx, vy)| Probe::new(Point(vx, vy)))
			.filter(|p| p.reaches_target(&self.target))
			.inspect(|p| eprintln!("{},{}", p.velocity.0, p.velocity.1))
			.count();

		Ok(valid as isize)
	}
}

#[test]
fn reference_velocities() {
	let target: Target = Target::from_raw([20, 30, -10, -5]);
	const VALID: &[Point] = &[
Point(23,-10),
Point(25,-9),
Point(27,-5),
Point(29,-6),
Point(22,-6),
Point(21,-7),
Point(9,0),
Point(27,-7),
Point(24,-5),
Point(25,-7),
Point(26,-6),
Point(25,-5),
Point(6,8),
Point(11,-2),
Point(20,-5),
Point(29,-10),
Point(6,3),
Point(28,-7),
Point(8,0),
Point(30,-6),
Point(29,-8),
Point(20,-10),
Point(6,7),
Point(6,4),
Point(6,1),
Point(14,-4),
Point(21,-6),
Point(26,-10),
Point(7,-1),
Point(7,7),
Point(8,-1),
Point(21,-9),
Point(6,2),
Point(20,-7),
Point(30,-10),
Point(14,-3),
Point(20,-8),
Point(13,-2),
Point(7,3),
Point(28,-8),
Point(29,-9),
Point(15,-3),
Point(22,-5),
Point(26,-8),
Point(25,-8),
Point(25,-6),
Point(15,-4),
Point(9,-2),
Point(15,-2),
Point(12,-2),
Point(28,-9),
Point(12,-3),
Point(24,-6),
Point(23,-7),
Point(25,-10),
Point(7,8),
Point(11,-3),
Point(26,-7),
Point(7,1),
Point(23,-9),
Point(6,0),
Point(22,-10),
Point(27,-6),
Point(8,1),
Point(22,-8),
Point(13,-4),
Point(7,6),
Point(28,-6),
Point(11,-4),
Point(12,-4),
Point(26,-9),
Point(7,4),
Point(24,-10),
Point(23,-8),
Point(30,-8),
Point(7,0),
Point(9,-1),
Point(10,-1),
Point(26,-5),
Point(22,-9),
Point(6,5),
Point(7,5),
Point(23,-6),
Point(28,-10),
Point(10,-2),
Point(11,-1),
Point(20,-9),
Point(14,-2),
Point(29,-7),
Point(13,-3),
Point(23,-5),
Point(24,-8),
Point(27,-9),
Point(30,-7),
Point(28,-5),
Point(21,-10),
Point(7,9),
Point(6,6),
Point(21,-5),
Point(27,-10),
Point(7,2),
Point(30,-9),
Point(21,-8),
Point(22,-7),
Point(24,-9),
Point(20,-6),
Point(6,9),
Point(29,-5),
Point(8,-2),
Point(27,-8),
Point(30,-5),
Point(24,-7),
	];

	for p in VALID {
		let probe = Probe::new(*p);
		if !probe.reaches_target(&target) {
			let points: Vec<(isize, isize)> = probe.iter_to(&target).map(|Point(x, y)| (x, y)).collect();
			panic!("{:?} does not intersect {:?}. (Points: {:?})", p, target, points)
		}
		assert!(Probe::new(*p).reaches_target(&target), "Point was unable to reach target: {:?}", p);
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = "
target area: x=20..30, y=-10..-5
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 45),
		(daystr!("17"), 30628),
	];
	test_runner::<Day17, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 112),
		(daystr!("17"), 4433),
	];
	test_runner::<Day17, _>(DayPart::Part2, &cases);
}
