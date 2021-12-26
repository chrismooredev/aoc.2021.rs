#![feature(const_generics_defaults)]

#![allow(unused_imports)]
use std::str::FromStr;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::{Graph, Undirected};
use rayon::prelude::*;
use indicatif::{ParallelProgressIterator, ProgressBar};

struct Volume<const N: usize>([usize; N]);
impl<const N: usize> Volume<N> {
	fn raw_volume(vol: [usize; N]) -> Self {
		Volume(vol)
	}

	/// The computed volume of this space. The product of all three dimensions.
	fn len(&self) -> usize {
		self.0.iter().product()
	}
}

struct VolumeTree<const N: usize>([isize; N]);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Cuboid<const N: usize = 3> {
	min: [isize; N],
	max: [isize; N],
}
impl Cuboid<3> {
	fn volume(&self) -> Volume<3> {
		let mut vol = [0; 3];
		for i in 0..3 {
			vol[i] = (self.max[i] - self.min[i]) as usize;
		}
		Volume(vol)
	}
	fn intersects(&self, other: &Cuboid<3>) -> bool {
		let [min_x1, min_y1, min_z1] = self.min;
		let [max_x1, max_y1, max_z1] = self.max;
		let [min_x2, min_y2, min_z2] = other.min;
		let [max_x2, max_y2, max_z2] = other.max;
		
		// https://gamedev.stackexchange.com/a/23751
		((min_x1 <= min_x2 && min_x2 <= max_x1) || (min_x2 <= min_x1 && min_x1 <= max_x2)) &&
		((min_y1 <= min_y2 && min_y2 <= max_y1) || (min_y2 <= min_y1 && min_y1 <= max_y2)) &&
		((min_z1 <= min_z2 && min_z2 <= max_z1) || (min_z2 <= min_z1 && min_z1 <= max_z2))
	}
}
impl<const N: usize> Cuboid<N> {
	fn contains(&self, coord: [isize; N]) -> bool {
		for i in 0..N {
			if !(self.min[i] <= coord[i] && coord[i] <= self.max[i]) {
				return false;
			}
		}
		true
	}

	/// Returns a cube specifying the mininum and maximum dimenions of the two
	/// cubes, even if they are disjoint.
	fn range(&self, other: &Cuboid<N>) -> Cuboid<N> {
		let mins: Vec<_> = self.min.iter()
			.zip(other.min.iter())
			.map(|(a, b)| a.min(b))
			.copied()
			.collect();
		let maxs: Vec<_> = self.max.iter()
			.zip(other.max.iter())
			.map(|(a, b)| a.max(b))
			.copied()
			.collect();
		Cuboid {
			min: mins.try_into().unwrap(),
			max: maxs.try_into().unwrap(),
		}
	}

	fn to_radius(&self) -> RadiusCube {
		let pairs: Vec<_> = self.min.iter()
			.zip(self.max.iter())
			.map(|(min, max)| (
				(max+min)/2, // center
				(max-min)/2, // radius
			))
			.collect();
		
		RadiusCube {
			center: [pairs[0].0, pairs[1].0, pairs[2].0],
			radius: [pairs[0].1, pairs[1].1, pairs[2].1],
		}
	}

	/// Returns true if `self` fully contains the `other` cube.
	fn fully_contains(&self, other: &Cuboid) -> bool {
		// minx1 <= minx2 <= maxx2 <= maxx1
		self.min.iter()
			.zip(other.min.iter())
			.zip(other.max.iter())
			.zip(self.max.iter())
			.all(|(((min1, min2), max2), max1)| {
				min1 <= min2 && max2 <= max1
			})
	}
}

struct RadiusCube {
	center: [isize; 3],
	radius: [isize; 3],
}
impl RadiusCube {
	fn as_string(&self) -> String {
		format!("({}±{}, {}±{}, {}±{})",
			self.center[0], self.radius[0],
			self.center[1], self.radius[1],
			self.center[2], self.radius[2],
		)	
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RebootInstr {
	state: bool,
	bounds: Cuboid,
}
impl RebootInstr {
	fn matches(&self, (x, y, z): (isize, isize, isize)) -> Option<bool> {
		let arg = [x, y, z];
		for i in 0..3 {
			if ! (self.bounds.min[i]..=self.bounds.max[i]).contains(&arg[i]) {
				return None;
			}
		}
		Some(self.state)
	}
	fn collides_with(&self, other: &RebootInstr) -> bool {
		self.bounds.intersects(&other.bounds)
	}

	fn as_graph(instrs: &[RebootInstr]) -> Graph<String, (), Undirected, u32> {
		let mut graph = Graph::new_undirected();

		let nodes: Vec<NodeIndex> = instrs.iter()
			.map(|ins| (ins.state, ins.bounds.to_radius()))
			.enumerate()
			.map(|(i, (st, rc))| format!("({}) {}:{}", i, if st { "on" } else { "off" }, rc.as_string()))
			.map(|st| graph.add_node(st))
			.collect();

		instrs.iter()
			.enumerate()
			.tuple_combinations()
			.filter(|((_, a), (_, b))| a.bounds.intersects(&b.bounds))
			.for_each(|((ia, _), (ib, _))| {
				graph.add_edge(nodes[ia], nodes[ib], ());
			});

		graph
	}

	fn sample(instrs: &[RebootInstr], point: [isize; 3]) -> bool {
		instrs.iter()
			.rev() // go backwards
			.find(|ri| ri.bounds.contains(point))
			.map(|ri| ri.state)
			.unwrap_or(false)
	}

	fn total_on(instrs: &[RebootInstr]) -> usize {

		eprintln!("sampling all axis points");

		let mut axes: [Vec<_>; 3] = Default::default();
		for ri in instrs.iter() {
			for axis in 0..3 {
				// add each pair [i, i)

				// the beginning of the inside of the cube
				axes[axis].push(ri.bounds.min[axis]);

				// add one so we can mark the beginning of
				// the outside of the cube
				axes[axis].push(ri.bounds.max[axis]+1);
			}
		}
		for axis in &mut axes {
			// so our .tuple_windows still emit the ending region
			axis.push(isize::MAX);

			// sorts neg -> pos
			axis.sort();

			// removes all entries at the same location - we are tracking changes
			axis.dedup();
		}

		let total_count = axes.iter().map(|v| v.len()).product::<usize>();
		eprintln!("sampling all regions ({} total)", total_count);
		
		let pb = ProgressBar::new(total_count as u64);
		pb.set_draw_rate(1);

		let mut c: u64 = 0;
		let pb_delta: u64 = (total_count as u64)/100; // 1% of the total

		// track each range (x, y, z) as a range of (low..high) of each region
		let regions = axes[0].iter().copied().tuple_windows()
			.cartesian_product(axes[1].iter().copied().tuple_windows())
			.cartesian_product(axes[2].iter().copied().tuple_windows())
			
			// flatten our tuples to an array
			.map::<[(isize, isize); 3], _>(|((x, y), z)| [x, y, z])

			// progress bar
			.inspect(|_| {
				c += 1;
				if c == pb_delta {
					pb.inc(pb_delta);
					c -= pb_delta;
				}
			})

			// make it parallel
			.par_bridge()

			// only keep active/on regions
			.filter(|&[(x, _), (y, _), (z, _)]| {
				RebootInstr::sample(instrs, [x, y, z])
			})

			// get region size per dimension
			.map(|[(x1, x2), (y1, y2), (z1, z2)]| {
				// the second ones should always be higher
				[x2 - x1, y2 - y1, z2 - z1]
			})

			// get region volume
			.map(|size| size.iter().product::<isize>() as usize)

			// sum all the volumes
			.sum::<usize>();

		pb.inc(c);
		pb.finish();

		regions
	}
}

pub struct Day22 {
	instrs: Vec<RebootInstr>
}
impl Day22 {
}

impl AoCDay for Day22 {
	type Answer = usize;

	fn day() -> u8 { 22 }
	fn name() -> &'static str { "Reactor Reboot" }

	fn parse(input: &str) -> DayResult<Self> {
		let instrs = aoch::parsing::from_lines_with(input, |line| {
			let (state, coords) = line.split_once(' ').unwrap();
			let state = match state {
				"on" => true,
				"off" => false,
				_ => panic!("unknown reactor state: {:?}", state),
			};
			let (x, y, z) = coords.split(',')
				.map(|s| &s.trim()[2..])
				.map(|s| s.split_once("..").unwrap())
				.map(|(lower, upper)| {
					(lower.parse().unwrap(), upper.parse().unwrap())
				})
				.collect_tuple().unwrap();

			RebootInstr {
				state,
				bounds: Cuboid {
					min: [x.0, y.0, z.0],
					max: [x.1, y.1, z.1],
				},
			}
		});
		Ok(Day22 { instrs })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		Ok(
			(-50..=50)
			.cartesian_product(-50..=50)
			.cartesian_product(-50..=50)
			.filter(|((x, y), z)| {
				self.instrs.iter()
					.rev()
					.find_map(|ri| ri.matches((*x, *y, *z)))
					.unwrap_or(false)
			})
			.count()
		)
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let collides = self.instrs.iter()
			.tuple_combinations()
			.filter(|(a, b)| {
				a.collides_with(b)
			})
			.count();
		let standalone = self.instrs.iter()
			.filter(|a| self.instrs.iter()
				.filter(|b| a != b)
				.all(|b| !a.collides_with(b))
			)
			.count();
		let combos = self.instrs.len()*(self.instrs.len()-1)/2;
		eprintln!("collides: {}, possible: {}, standalone: {}", collides, combos, standalone);

		let bounds = self.instrs.iter()
			.filter(|c| c.state)
			.map(|c| c.bounds)
			.reduce(|cube, other| cube.range(&other))
			.unwrap();
		
		eprintln!("bounds: {:?}, volume: {} units cubed", bounds, bounds.volume().len());

		let total_sum: usize = self.instrs.iter()
			.map(|ins| ins.bounds.volume().len())
			.sum();
		eprintln!("sum volume of each cube: {} ({}x of total)", total_sum, (total_sum as f64)/(bounds.volume().len() as f64));
		
		// take pairs, and test for intersections?
		let intersecting = self.instrs.iter()
			.tuple_combinations()
			.filter(|(a, b)| a.bounds.intersects(&b.bounds))
			.count();
		eprintln!("intersecting combinations: {}, total possible: {}", intersecting, self.instrs.len()*(self.instrs.len()-1)/2);

		let containing = self.instrs.iter()
			.tuple_combinations()
			.filter(|(a, b)| a.bounds.fully_contains(&b.bounds))
			.count();
		eprintln!("cubes completely covering others: {}, total possible: {}", containing, self.instrs.len()*(self.instrs.len()-1)/2);


		let graph = RebootInstr::as_graph(&self.instrs);
		let dot = Dot::new(&graph);
		let dotstr = format!("{:?}", dot);
		let _ = std::fs::write("reactor_graph.dot", dotstr);

		Ok(RebootInstr::total_on(&self.instrs))
	}
}

#[cfg(test)]
const TEST_INPUT1: &'static str = "
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
";

#[cfg(test)]
const TEST_INPUT2: &'static str = "
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682
";

#[cfg(test)]
const TEST_INPUT3: &str = "
on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507
";

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT1, 39),
		(TEST_INPUT2, 590784),
		(TEST_INPUT3, 474140),
		(daystr!("22"), 652209),
	];
	test_runner::<Day22, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT3, 2758514936282235),
		(daystr!("22"), 1217808640648260),
	];
	test_runner::<Day22, _>(DayPart::Part2, &cases);
}
