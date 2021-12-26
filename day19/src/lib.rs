#![feature(int_abs_diff)]
#![allow(clippy::type_complexity)]

use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::Hasher;
use std::ops::{Neg, self};
use std::fmt;
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};
use num_enum::{TryFromPrimitive, IntoPrimitive};
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::visit::{Bfs, Walker, EdgeRef};
use petgraph::{Graph, Undirected};

macro_rules! impl_triple_debug {
	($t: ty) => {
		impl fmt::Debug for $t {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				let [x, y, z] = &self.0;
				f.write_str("(")?;
				fmt::Debug::fmt(x, f)?;
				f.write_str(", ")?;
				fmt::Debug::fmt(y, f)?;
				f.write_str(", ")?;
				fmt::Debug::fmt(z, f)?;
				f.write_str(")")
			}
		}
	}
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Point([isize; 3]);
impl Point {
	fn abs_diff(&self, other: &Point) -> Magnitude {
		Magnitude([
			self.0[0].abs_diff(other.0[0]),
			self.0[1].abs_diff(other.0[1]),
			self.0[2].abs_diff(other.0[2]),
		])
	}
	fn diff(&self, other: &Point) -> Difference {
		// may want to swap these around
		Difference([
			self.0[0] - other.0[0],
			self.0[1] - other.0[1],
			self.0[2] - other.0[2],
		])
	}
}
impl_triple_debug!(Point);
impl ops::Neg for Point {
    type Output = Point;
    fn neg(self) -> Self::Output {
		let [x, y, z] = self.0;
        Point([-x, -y, -z])
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Difference([isize; 3]);
impl Difference {
	const ZERO: Difference = Difference([0, 0, 0]);

	fn as_point(&self) -> Point {
		Point(self.0)
	}
}
impl_triple_debug!(Difference);
impl ops::Add for Difference {
    type Output = Difference;
    fn add(self, rhs: Difference) -> Self::Output {
		let [sx, sy, sz] = self.0;
		let [ox, oy, oz] = rhs.0;
        Difference([sx+ox, sy+oy, sz+oz])
    }
}

struct DifferenceMatcher {
	this: [isize; 3],
	other: [isize; 3],
	thisabs: [usize; 3],
	otherabs: [usize; 3],
	step: usize,
}
impl Iterator for DifferenceMatcher {
	type Item = CoordinateMap;
	fn next(&mut self) -> Option<Self::Item> {
		use AxisMapping::*;
		let [bx, by, bz] = self.otherabs;
		loop {
			let orient = match self.step {
				0 if self.thisabs == [bx, by, bz] => XYZ,
				1 if self.thisabs == [bx, bz, by] => XZY,
				2 if self.thisabs == [by, bx, bz] => YXZ,
				3 if self.thisabs == [by, bz, bx] => YZX,
				4 if self.thisabs == [bz, bx, by] => ZXY,
				5 if self.thisabs == [bz, by, bx] => ZYX,
				0..=5 => {
					// no matches found, go 'round again, trying the next
					self.step += 1;
					continue;
				},
				_ => { return None; },
			};
			self.step += 1;

			let [mx, my, mz] = orient.map(&self.other);

			// check that abs'ing our mapped relative offsets
			// match our original abs offsets
			assert_eq!(mx.abs() as usize, self.thisabs[0]);
			assert_eq!(my.abs() as usize, self.thisabs[1]);
			assert_eq!(mz.abs() as usize, self.thisabs[2]);

			// if the signs don't match, they need to be inverted
			let invert = [
				self.this[0].signum() != self.other[0].signum(),
				self.this[1].signum() != self.other[1].signum(),
				self.this[2].signum() != self.other[2].signum(),
			];

			return Some(CoordinateMap {
				axis: orient,
				invert,
			});
		}
	}
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Magnitude([usize; 3]);
impl Magnitude {
	fn sum(&self) -> usize {
		self.0.iter().sum()
	}
}
impl_triple_debug!(Magnitude);

/// Returns magnitudes that would be valid to turn `other` into `this`
struct MagnitudeMatcher {
	this: [usize; 3],
	other: [usize; 3],
	step: usize,
}
impl Iterator for MagnitudeMatcher {
	type Item = AxisMapping;
	fn next(&mut self) -> Option<Self::Item> {
		use AxisMapping::*;
		let [bx, by, bz] = self.this;
		loop {
			let orient = match self.step {
				0 if self.other == [bx, by, bz] => XYZ,
				1 if self.other == [bx, bz, by] => XZY,
				2 if self.other == [by, bx, bz] => YXZ,
				3 if self.other == [by, bz, bx] => YZX,
				4 if self.other == [bz, bx, by] => ZXY,
				5 if self.other == [bz, by, bx] => ZYX,
				0..=5 => {
					// no matches found, go 'round again, trying the next
					self.step += 1;
					continue;
				},
				_ => { return None; },
			};
			self.step += 1;
			return Some(orient);
		}
	}
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct CoordinateMap {
	axis: AxisMapping,
	invert: [bool; 3],
}
impl CoordinateMap {
	const IDENTITY: CoordinateMap = CoordinateMap {
		axis: AxisMapping::XYZ,
		invert: [false, false, false],
	};
	const TOTAL: usize = 6*(2usize.pow(3));

	// (0..CoordinateMap::TOTAL)
	fn from_index(idx: usize) -> CoordinateMap {
		// if idx >= CoordinateMap::TOTAL { panic!("coordinate map index out of range, must be within 0..24 (got {})", idx); }
		assert!((0..CoordinateMap::TOTAL).contains(&idx));
		let axis_map_idx = idx >> 3;
		let axis = AxisMapping::try_from(axis_map_idx).unwrap();
		let inv = idx & 0b111;
		let invert = [inv & 0b100 != 0, inv & 0b010 != 0, inv & 0b001 != 0];

		CoordinateMap {
			axis,
			invert,
		}
	}

	fn map<T: Clone + Neg<Output = T>>(&self, m: &[T; 3]) -> [T; 3] {
		let [mx, my, mz] = self.axis.map_rev(m);
		[
			if self.invert[0] { -mx } else { mx },
			if self.invert[1] { -my } else { my },
			if self.invert[2] { -mz } else { mz },
		]
	}
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, IntoPrimitive, TryFromPrimitive)]
#[repr(usize)]
#[allow(clippy::upper_case_acronyms)]
enum AxisMapping {
	XYZ,
	XZY,
	YXZ,
	YZX,
	ZXY,
	ZYX,
}
impl AxisMapping {
	/// Maps a triple. Interprets input `m` as XYZ and outputs as this map.
	fn map<T: Clone>(&self, m: &[T; 3]) -> [T; 3] {
		use AxisMapping::*;
		let [mx, my, mz] = m.clone();
		match self {
			XYZ => [mx, my, mz],
			XZY => [mx, mz, my],
			YXZ => [my, mx, mz],
			YZX => [my, mz, mx],
			ZXY => [mz, mx, my],
			ZYX => [mz, my, mx],
		}
	}

	/// Maps a triple. Interprets input as this map, and outputs as XYZ
	fn map_rev<T: Clone>(&self, m: &[T; 3]) -> [T; 3] {
		use AxisMapping::*;
		match self {
			XYZ => { let [mx, my, mz] = m.clone(); [mx, my, mz] },
			XZY => { let [mx, mz, my] = m.clone(); [mx, my, mz] },
			YXZ => { let [my, mx, mz] = m.clone(); [mx, my, mz] },
			YZX => { let [my, mz, mx] = m.clone(); [mx, my, mz] },
			ZXY => { let [mz, mx, my] = m.clone(); [mx, my, mz] },
			ZYX => { let [mz, my, mx] = m.clone(); [mx, my, mz] },
		}
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PointTranslation {
	other_axis: CoordinateMap,
	offset: Difference,
}
impl PointTranslation {
	const IDENTITY: PointTranslation = PointTranslation::new(CoordinateMap::IDENTITY, Difference::ZERO);
	const fn new(map: CoordinateMap, offset: Difference) -> PointTranslation {
		PointTranslation {
			other_axis: map,
			offset
		}
	}
	fn map(&self, p: Point) -> Point {
		let mut kk = Point(self.other_axis.map(&p.0));
		kk.0[0] += self.offset.0[0];
		kk.0[1] += self.offset.0[1];
		kk.0[2] += self.offset.0[2];
		kk
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RawScanner {
	idx: usize,
	/// The raw points
	raw: Vec<Point>,
	/// For each raw point, a list of distances to other points, a sorted list
	/// of absolute differences on each axis, the absolute differences by axis,
	/// and the specific point matched against.
	/// 
	/// This list is sorted by distances; shorter first.
	/// Does not include the initial point.
	distances: HashMap<Point, Vec<(usize, [usize; 3], Difference, Point)>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Scanner {
	idx: usize,
	// the raw points
	raw: Vec<Point>,

	// as_raw: &'rs RawScanner,

	// normalized to the orientation and position of scanner 0
	normal_pos: Point,
	normal: Vec<Point>,
	normal_distances: HashMap<Point, Vec<(usize, [usize; 3], Difference, Point)>>,

	/// raw + offset = normal
	translation: PointTranslation,
}

impl RawScanner {
	fn new(idx: usize, raw: Vec<Point>) -> RawScanner {
		let mut raw = raw;
		raw.sort_unstable();

		let distances = RawScanner::compute_distances(&raw);

		RawScanner {
			idx, raw,
			distances,
		}
	}

	fn upgrade_root(&self) -> Scanner {
		Scanner {
			// as_raw: self,
			idx: self.idx,
			raw: self.raw.clone(),
			normal_pos: Point([0, 0, 0]),
			normal: self.raw.clone(),
			normal_distances: RawScanner::compute_distances(&self.raw),
			translation: PointTranslation::IDENTITY,
		}
	}
	fn normalize_to(&self, other: &Scanner, breadth: usize) -> Scanner {
		// find a way to map the coordinates

		// eprintln!("attempting to normalize {} to {}", self.idx, other.idx);

		// our current, raw hash -> point map
		let self_hash: HashMap<u64, Point> = self.raw.iter()
			.map(|p| (self.hash_point(p, breadth).unwrap(), *p))
			.collect();

		// their current, raw hash -> point map
		let ref_hash: HashMap<u64, Point> = other.normal.iter()
			.map(|p| (other.normal_hash_point(p, breadth).unwrap(), *p))
			.collect();

		let joined_hash_self_ref: HashMap<u64, (Point, Point)> = self_hash.iter()
			.filter_map(|(h, p)| {
				let other = ref_hash.get(h)?;
				Some((*h, (*p, *other)))
			})
			.collect();

		let mut point_pairs: Vec<(Point, Point)> = joined_hash_self_ref.values()
			.copied()
			.collect();
			
		point_pairs.sort_unstable();

		let possible_axis_transforms = (0..CoordinateMap::TOTAL)
			.map(CoordinateMap::from_index)
			.filter_map(|cm| {
				// test if the coordinate map works, and find the corresponding
				// point offset after translation
				let offsets: HashSet<Difference> = point_pairs.iter()
					.map(|(sp, rp)| {
						let self_dists = &self.distances.get(sp).unwrap()[..breadth];
						let refe_dists = &other.normal_distances.get(rp).unwrap()[..breadth];

						// test this coordinate map, and compute point offsets
						let mut valid_offsets = self_dists.iter().zip(refe_dists)
							.map(|(&(_, _, self_diff, _self_point), &(_, _, ref_diff, _ref_point))| {
								let exp_ref_diff = Difference(cm.map(&self_diff.0));
								if exp_ref_diff == ref_diff {
									// coordinate axis are now aligned - get offset?
									let transed = Point(cm.map(&sp.0));
									let diff = rp.diff(&transed);
									Some(diff)
								} else {
									None
								}
							});

						let offset: Difference = match valid_offsets.next() {
							None => return None, // no coordinates
							Some(None) => return None, // found a non-match, bad coordinate map
							Some(Some(diff)) => diff,
						};

						for element in valid_offsets {
							match element {
								None => return None, // found a non-match, bad coordinate map
								Some(ndiff) if ndiff == offset => {}, // good
								Some(_ndiff) => panic!("non-constant point offsets between scanner {} and {}", self.idx, other.idx), // found a non-match, odd coordinates?
							}
						}

						Some(offset)
					})
					.collect::<Option<_>>()?;
				
				// they should all have the same offset
				assert_eq!(1, offsets.len());

				Some((cm, offsets.iter().next().copied().unwrap()))
			})
			.collect::<Vec<_>>();

		// eprintln!("possible transformations: {:?}", possible_axis_transforms);
		assert_eq!(1, possible_axis_transforms.len(), "more than one possible axis transforms!");
		let (coordmap, offset) = possible_axis_transforms[0];
		let translation = PointTranslation::new(coordmap, offset);

		let mut transformed_self_points: Vec<Point> = self.raw.iter()
			.map(|p| translation.map(*p))
			.collect();

		let mut other_as_hash: Vec<Point> = other.normal.iter()
			.copied()
			.collect();

		transformed_self_points.sort_unstable();
		other_as_hash.sort_unstable();

		assert_eq!(self.raw.len(), transformed_self_points.len());
		assert_eq!(other.raw.len(), other_as_hash.len());

		let self_mapped: HashSet<Point> = self.raw.iter().map(|p| translation.map(*p)).collect();
		assert_eq!(self.raw.len(), self_mapped.len());
		// let other_set: HashSet<Point> = other.normal.iter().copied().collect();
		// eprintln!("common points after local transformation: {} ({} in self, {} in other)", self_mapped.intersection(&other_set).count(), self_mapped.len(), other_set.len());

		// since we based our own translation off our 'parent's normal points,
		// we don't need further translation
		let normalized = self.normalize(translation);
		let self_mapped: HashSet<Point> = normalized.normal.iter().copied().collect();
		let other_set: HashSet<Point> = other.normal.iter().copied().collect();
		let common_mapped = self_mapped.intersection(&other_set).count();
		assert!(common_mapped >= 12, "not enough matching points between adjacent scanners after normalization! (scanners {} and {}, found {} matching points)", self.idx, other.idx, common_mapped);
		// eprintln!("Done. common points after transformation: {} ({} in self, {} in other)", self_mapped.intersection(&other_set).count(), self_mapped.len(), other_set.len());

		normalized
	}
	fn normalize(&self, translation: PointTranslation) -> Scanner {
		let points: Vec<Point> = self.raw.iter().map(|p| translation.map(*p)).collect();
		let scan = Scanner {
			idx: self.idx,
			raw: self.raw.clone(),
			// as_raw: self,
			normal_pos: -translation.offset.as_point(), // is negation correct?
			normal_distances: RawScanner::compute_distances(&points),
			normal: points,
			translation,
		};

		assert_eq!(scan.raw.len(), scan.normal.len());
		scan
	}

	// this is so fast we can just calculate it whenever we need it
	fn hash_point(&self, point: &Point, breadth: usize) -> Option<u64> {
		Day19::hash_point(point, breadth, &self.distances)
	}

	fn compute_distances(points: &[Point]) -> HashMap<Point, Vec<(usize, [usize; 3], Difference, Point)>> {
		points.iter()
			.copied()
			.map(|src| {
				let mut dists = points.iter()
					.copied()
					.map(|dst| {
						let ad = src.abs_diff(&dst);
						let mut axis = ad.0;
						axis.sort_unstable();
						(ad.sum(), axis, src.diff(&dst), dst)
					})
					.collect::<Vec<_>>();
				dists.sort_unstable();
				assert!(
					matches!(dists[0], (0, [0,0,0], Difference([0,0,0]), _))
				);
				dists.remove(0);
				(src, dists)
			})
			.collect()
	}
}
impl Scanner {
	fn normal_hash_point(&self, point: &Point, breadth: usize) -> Option<u64> {
		Day19::hash_point(point, breadth, &self.normal_distances)
	}
}

pub struct Day19 {
	raw_scanners: Vec<RawScanner>,
	normalized: Option<(
		Vec<Scanner>,
		Graph<String, usize, Undirected, u32>,
		Vec<NodeIndex>,
	)>,
}
impl Day19 {
	fn hash_point(point: &Point, breadth: usize, dists: &HashMap<Point, Vec<(usize, [usize; 3], Difference, Point)>>) -> Option<u64> {
		
		// the 'hash' has to not include any relative distances
		// this includes signs, or axis placements
		// the hash /can/ include the raw numbers, in a specific order

		// hash `breadth` is how many points to include
		// each additional 'hash' includes total distance sum, and sorted,
		// absolute distances - irregardless of XYZ axis

		let hasher = dists.get(point)?
			.iter()
			.take(breadth)
			.fold(DefaultHasher::new(), |mut acc, (dist, dists, _, _)| {
				// note that `dists` is already sorted
				// from least to greatest
				acc.write_usize(*dist);
				dists.iter()
					.for_each(|a| acc.write_usize(*a));
				acc
			});
		let hash = hasher.finish();
		Some(hash)
	}

	/// Creates a graph of nearby scanners, based on common point hashes
	fn graph_scanners(raw_scanners: &[RawScanner], hash_breadth: usize, required_common_points: usize)
		-> Option<(Graph<String, usize, Undirected, u32>, Vec<NodeIndex>)>
	{
		// get hashes of points within each scanner
		let scanner_hashes: Vec<HashSet<u64>> = raw_scanners.iter()
			.map::<HashSet<_>, _>(|rs| {
				rs.raw.iter()
					.map(|p| rs.hash_point(p, hash_breadth))
					.map(Option::unwrap)
					.collect()
			})
			.collect();

		// try to match hashes between scanners, use matches as graph edges
		let mut node_idxs = vec![None; raw_scanners.len()];
		let mut connected = Graph::new_undirected();

		scanner_hashes.iter()
			.enumerate()
			.tuple_combinations()
			.for_each(|((ia, a), (ib, b))| {
				// How many points have the same hash across these two lists
				let common = a.intersection(b).count();
				if common < required_common_points { return; }

				// get the nodes
				let mut node_idx = |idx: usize| -> NodeIndex {
					*node_idxs[idx].get_or_insert_with(|| {
						connected.add_node(format!(
							"Scanner {} ({} points)",
							idx, raw_scanners[idx].raw.len()
						))
					})
				};

				let na = node_idx(ia);
				let nb = node_idx(ib);

				connected.add_edge(na, nb, common);
				
				// eprintln!(
				// 	"hash_size[{}](scnr {} <-> {}) = {} common points",
				// 	hash_breadth, ia, ib, common,
				// );
			});

		// we can have all nodes in our graph, but not necessarily all
		// connected - so walk the graph and count what is
		// found starting from scanner 0
		let reachable = Bfs::new(&connected, node_idxs[0]?)
			.iter(&connected)
			// .inspect(|s| { eprintln!("\tvisiting node: {:?}", s) })
			.count();


		if connected.edge_count() > 0 {
			let name = format!("day19_bounds{}.dot", hash_breadth);
			let dot = Dot::new(&connected); //, &[Config::EdgeNoLabel, Config::NodeIndexLabel]);
			if let Err(e) = std::fs::write(&name, dot.to_string()) {
				eprintln!("[B={}] failed writing scanner graph to disk: {:?}", hash_breadth, e);
			} else {
				eprintln!("[B={}] wrote scanner graph to disk", hash_breadth);
			}
		}

		if reachable == 0 { return None; }
		// eprintln!(
		// 	"[B={}] reachable scanners from scanner 0: {} (total nodes={}, total scanners={})",
		// 	hash_breadth, reachable, connected.node_count(), raw_scanners.len()
		// );
		if reachable != raw_scanners.len() { return None; }

		let node_idxs = node_idxs.iter()
			.copied()
			.map(Option::unwrap)
			.collect();

		Some((connected, node_idxs))
	}

	fn normalize_scanners(&mut self) -> &(
		Vec<Scanner>,
		Graph<String, usize, Undirected, u32>,
		Vec<NodeIndex>,
	) {
		let raw_scanners = &self.raw_scanners;
		self.normalized.get_or_insert_with(|| {
			const REQUIRED_COMMON_POINTS: usize = 12;
			let scanners = self.raw_scanners.len();

			let min_point_len = self.raw_scanners.iter()
				.map(|rs| rs.raw.len())
				.min()
				.unwrap();

			'breadth: for pb in 1..min_point_len-1 {
				
				// assert no hash collisions on each scanner
				for rs in &self.raw_scanners {
					let hs: HashSet<u64> = rs.raw.iter()
						.map(|p| rs.hash_point(p, pb).unwrap())
						.collect();
					if rs.raw.len() != hs.len() {
						eprintln!("[B={}] SKIPPING -- hash collisions (expected {}, got {} for scanner {})", pb, rs.raw.len(), hs.len(), rs.idx);
						continue 'breadth;
					}
				}

				// eprintln!("[B={}] all point hashes unique per scanner", pb);

				// Find a graph of connected scanners that we can walk through
				// use common hashes of points to identify common scanners
				let (graph, node_idxs) = match Day19::graph_scanners(raw_scanners, pb, REQUIRED_COMMON_POINTS) {
					None => {
						eprintln!("[B={}] SKIPPING -- could not form continuous graph", pb);
						continue;
					},
					Some(s) => s,
				};

				// eprintln!("[B={}] continuous scanner graph found", pb);

				let try_as_raw = |idx: NodeIndex| -> Option<(usize, &RawScanner)> {
					node_idxs.iter()
						.enumerate()
						.find(|(_, ni)| **ni == idx)
						.map(|(i, _)| (i, &self.raw_scanners[i]))
				};
				let as_raw = |idx: NodeIndex| -> (usize, &RawScanner) {
					try_as_raw(idx).unwrap()
				};

				// walk through each scanner, going out from the root node
				// since we are always going to have a previous node, we can just
				// add any offsets between the last node and this to receive
				// an offset to the root
				let mut bfs = Bfs::new(&graph, node_idxs[0]).iter(&graph);
				let mut mapped: Vec<Option<Scanner>> = vec![None; scanners];

				let first = bfs.next().unwrap();
				let (si0, s0) = as_raw(first);
				assert_eq!(0, si0, "first visited node should be initial scanner");
				mapped[0] = Some(s0.upgrade_root());
				// mapped[0] = Some(s0.clone().normalize(()));

				for node_idx in bfs {
					// the scanner corresponding to this node
					let (i, rscanner) = as_raw(node_idx);

					// previous scanner we've done attached to this one
					let (_psi, prev_scanner) = graph.edges(node_idx)
						.map(|e| {
							// go through all edges connected to this node, and get
							// the opposite node
							let a = e.source();
							if a != node_idx { a } else { e.target() }
						})
						// find the first node that we've already done
						.filter_map(try_as_raw)
						// get the processed version, there should be at least one
						.find_map(|(i, _rs)| mapped[i].as_ref().map(|s| (i, s)))
						.unwrap();

					let normalized = rscanner.normalize_to(prev_scanner, pb);
					mapped[i] = Some(normalized);
				}

				let normalized: Vec<Scanner> = mapped.into_iter()
					.flatten()
					.collect();
				assert_eq!(self.raw_scanners.len(), normalized.len());

				// eprintln!("normalized all scanners");

				return (normalized, graph, node_idxs);
			}
			panic!("found no good point breadth for hashing");
		})
	}
}

impl AoCDay for Day19 {
	type Answer = usize;

	fn day() -> u8 { 19 }
	fn name() -> &'static str { "Beacon Scanner" }

	fn parse(input: &str) -> DayResult<Self> {
		let raw_scanners: Vec<RawScanner> = input.split("\n\n")
			.map(|s| {
				// eprintln!("{:?}", s);
				let mut iter = s.lines().filter_map(aoch::parsing::trimmed);
				
				// "--- scanner N ---" header
				let header = iter.next().unwrap();
				let num_str = &header[12..header.len()-4];
				let idx = num_str.parse()
					.expect("unable to parse scanner number");

				let raw: Vec<Point> = iter
					.map(|l| {
						// eprintln!("pair: {:?}", l);
						l.split(',')
							.map(|s| s.parse().unwrap())
							.collect::<Vec<isize>>()
							.try_into().unwrap()
					})
					.map(Point)
					.collect();
				
				RawScanner::new(idx, raw)
			})
			.collect();
		
		Ok(Day19 { raw_scanners, normalized: None })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let (normalized, _, _) = self.normalize_scanners();

		// total number of beacons

		let all_points: HashSet<Point> = normalized.iter()
			.map(|s| &s.normal)
			.flatten()
			.copied()
			.collect();

		Ok(all_points.len())
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let (normalized, _, _) = self.normalize_scanners();

		// largest manhattan difference between any two scanners

		Ok(normalized.iter()
			.map(|s| s.normal_pos)
			.tuple_combinations()
			.map(|(sa, sb)| {
				sa.abs_diff(&sb).sum()
			})
			.max().unwrap())
	}
}

#[cfg(test)]
const TEST_INPUT: &'static str = include_str!("../../input/19.ex.txt");

#[test]
fn part1() {
	let cases = [
		(TEST_INPUT, 79),
		(daystr!("19"), 335),
	];
	test_runner::<Day19, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		(TEST_INPUT, 3621),
		(daystr!("19"), 10864), // not 3621
	];
	test_runner::<Day19, _>(DayPart::Part2, &cases);
}
