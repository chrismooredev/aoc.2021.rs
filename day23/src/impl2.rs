use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::ops::{Index, IndexMut, Range};

use crate::MoveCount;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Pod {
	A,
	B,
	C,
	D,
}
impl Pod {
	pub fn value(self) -> usize {
		match self {
			Pod::A => 1,
			Pod::B => 10,
			Pod::C => 100,
			Pod::D => 1000,
		}
	}
	pub fn chr(self) -> char {
		match self {
			Pod::A => 'A',
			Pod::B => 'B',
			Pod::C => 'C',
			Pod::D => 'D',
		}
	}
    pub fn try_from_char(value: char) -> Result<Option<Pod>, ParseGameStateError> {
        match value {
            '.' => Ok(None),
            'A' => Ok(Some(Pod::A)),
            'B' => Ok(Some(Pod::B)),
            'C' => Ok(Some(Pod::C)),
            'D' => Ok(Some(Pod::D)),
            _ => Err(ParseGameStateError),
        }
    }
    pub fn try_from_u8(value: u8) -> Option<Pod> {
        match value {
            0 => Some(Pod::A),
            1 => Some(Pod::B),
            2 => Some(Pod::C),
            3 => Some(Pod::D),
            _ => None,
        }
    }
    pub fn entrance(self) -> u8 {
        2 + (self as u8)*2
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParseGameStateError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GameState2<const D: usize, const R: usize>
    where [(); 3+R*2]:,
{
    hall: [Option<Pod>; 3+R*2],
    rooms: [[Option<Pod>; D]; R],
}

impl<const D: usize, const R: usize> GameState2<D, R>
    where [(); 3+R*2]:
{
    pub fn from_solid(s: [[Pod; D]; R]) -> Self {
        // copy all the values over
        let mut rooms = [[None; D]; R];
        for ri in 0..R {
            for di in 0..D {
                rooms[ri][di] = Some(s[ri][di]);
            }
        }

        GameState2 {
            hall: [None; 3+R*2],
            rooms,
        }
    }
    pub fn try_parse(s: &str) -> Result<Self, ParseGameStateError> {
        let lines: Vec<&str> = s.lines().filter(|s| !s.is_empty()).collect();
        
        // let hall_length = 5+R*2;
        let mut hall: [Option<Pod>; 3+R*2] = [None; 3+R*2];
        let mut rooms: [[Option<Pod>; D]; R] = [[None; D]; R];
        
        for (i, l) in lines.iter().copied().map(str::trim).enumerate() {
            // assert line lengths
            if (i < 3 && l.len() != 2+3+R*2) || (i >= 3 && l.len() != 1+R*2) {
                return Err(ParseGameStateError);
            }
            
            let l = l.trim_matches('#');

            // assert trimmed line lengths
            if i == 0 {
                // should be only hashes
                if !l.is_empty() { return Err(ParseGameStateError); }
            } else if i == 1 {
                // hallway line
                if l.len() != 3+R*2 { return Err(ParseGameStateError); }
                
                for (ir, c) in l.chars().enumerate() {
                    if let Some(a) = Pod::try_from_char(c)? {
                        assert!(hall[ir].is_none());
                        hall[ir] = Some(a);
                    }
                }
            } else if i == lines.len()-1 {
                // should be only hashes
                if !l.is_empty() { return Err(ParseGameStateError); }
            } else {
                // deep room lines
                if l.len() != R*2-1 { return Err(ParseGameStateError); }

                for (ir, c) in l.chars().enumerate() {
                    if c == '#' { continue; }
                    if let Some(a) = Pod::try_from_char(c)? {
                        let room = &mut rooms[ir/2];
                        let depth = i-2;
                        assert!(room[depth].is_none());
                        room[depth] = Some(a);
                    }
                }
            }
        }

        Ok(GameState2 { hall, rooms })
    }

    /// Tests if a path between `src` and `dst` is available. If so, returns the length of the path.
    /// 
    /// Ignores any Amphipod on the 'source' tile, if it exists.
    pub fn path_available(&self, src: Position, dst: Position) -> Option<u8> {
        let room_and_hallway = |hall_is_src: bool, hallway_ind: u8, pod: Pod, depth: u8| -> Option<u8> {
            let room_hall_pos = pod.entrance();
            let room_hall = Position::Hallway(room_hall_pos as u8);
            let hall_len = if hall_is_src {
                self.path_available(Position::Hallway(hallway_ind), room_hall)?
            } else {
                self.path_available(room_hall, Position::Hallway(hallway_ind))?
            };

            // plus one to turn depth zero-based index to length
            let room_travel_len = depth + 1;
            Some(hall_len + room_travel_len)
        };

        match (src, dst) {
            (Position::Hallway(s), Position::Hallway(d)) => {
                let (lower, greater) = match s.cmp(&d) {
                    Ordering::Equal => return Some(0),
                    Ordering::Less => (s+1, d),
                    Ordering::Greater => (d, s-1),
                };

                if self.hall[lower as usize..greater as usize].iter().any(Option::is_some) {
                    // there is a thing in the way
                    return None;
                }

                Some(s.abs_diff(d))
            },
            (Position::Room { pod, depth, }, Position::Room { .. }) => {
                // reduce to room + (hallway -> room)

                // plus one to turn depth zero-based index to length
                let room_travel_len = depth + 1;

                // src pod entrance
                let room_hall_pos = pod.entrance();
                let room_hall = Position::Hallway(room_hall_pos as u8);

                let hall_dst_room_len = self.path_available(room_hall, dst)?;

                Some(hall_dst_room_len + room_travel_len)
            },
            (Position::Hallway(s), Position::Room { pod, depth }) => room_and_hallway(true,  s, pod, depth),
            (Position::Room { pod, depth }, Position::Hallway(s)) => room_and_hallway(false, s, pod, depth),
        }
    }

    /// An iterator of possible moves that could come after this state.
    pub fn movable(&self) -> MoveIter<D, R> {
        MoveIter::new(*self)
    }
    
    /// An iterator of move counts, representing possible paths to the solution of this puzzle.
    pub fn solutions(&self) -> Solutions<D, R> {
        Solutions::new(*self)
    }

    fn test_room(&self, pod: Pod) -> RoomStatus {
        RoomStatus::test(self.rooms[pod as usize], pod)
    }

    #[must_use]
    pub fn play(&self, mov: &Move) -> Self {
        let mut next = *self;
        let src = next[mov.src];
        let dst = next[mov.dst];
        debug_assert!(src.is_some());
        debug_assert!(dst.is_none());
        
        next[mov.dst] = next[mov.src];
        next[mov.src] = None;

        next
    }

    pub fn has_won(&self) -> bool {
        for r in 0..R {
            for d in 0..D {
                match self.rooms[r][d] {
                    Some(p) if (p as usize == r) => continue,
                    _ => return false,
                }
            }
        }
        
        true
    }
}

impl<const D: usize, const R: usize> fmt::Display for GameState2<D, R>
    where [(); 3+R*2]:
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const WALL: char = '#';

        // top wall
        for _ in 0..5+R*2 {
            f.write_char(WALL)?;
        }
        f.write_char('\n')?;

        // hallway
        f.write_char(WALL)?;
        for oa in self.hall {
            f.write_char(oa.map(Pod::chr).unwrap_or('.'))?;
        }
        f.write_char(WALL)?;
        f.write_char('\n')?;

        // intermediate room layers
        for d in 0..D {
            f.write_str(if d == 0 { "###" } else { "  #" })?;
            for ri in 0..R {
                f.write_char(self.rooms[ri][d].map(Pod::chr).unwrap_or('.'))?;
                f.write_char('#')?;
            }
            if d == 0 { f.write_str("##")?; }
            f.write_char('\n')?;
        }

        // bottom wall
        f.write_str("  ")?;
        for _ in 0..1+R*2 {
            f.write_char('#')?;
        }

        Ok(())
    }
}
impl<const D: usize, const R: usize> Index<Position> for GameState2<D, R>
    where [(); 3+R*2]:
{
    type Output = Option<Pod>;
    fn index(&self, index: Position) -> &Self::Output {
        match index {
            Position::Hallway(h) => &self.hall[h as usize],
            Position::Room { pod, depth } => &self.rooms[pod as usize][depth as usize]
        }
    }
}
impl<const D: usize, const R: usize> IndexMut<Position> for GameState2<D, R>
    where [(); 3+R*2]:
{
    fn index_mut(&mut self, index: Position) -> &mut Self::Output {
        match index {
            Position::Hallway(h) => &mut self.hall[h as usize],
            Position::Room { pod, depth } => &mut self.rooms[pod as usize][depth as usize]
        }
    }
}

/// An internal state machine for MoveIter
/// New -> Hallway
/// Hallway -> Room
/// Room -> RoomToHallway
/// Room -> Done
/// RoomToHallway -> Room
#[derive(Debug, Clone)]
enum MoveIterState {
    Hallway(u8),
    Room { room: u8, depth: u8 },
    RoomToHallway {
        next_room: u8,
        src_depth: u8,
        src_hall_slot: u8,
        src: Position,
        range: Range<u8>,
        pod: Pod,
    },
    Done,
}

#[derive(Debug, Clone)]
pub struct MoveIter<const D: usize, const R: usize>
    where [(); 3+R*2]:
{
    state: GameState2<D, R>,
    next: MoveIterState,
}
impl<const D: usize, const R: usize> MoveIter<D, R>
    where [(); 3+R*2]:
{
    fn new(state: GameState2<D, R>) -> MoveIter<D, R> {
        // eprintln!("creating MoveIter for state:\n{}", state);
        MoveIter {
            state,
            next: MoveIterState::Hallway(0),
        }
    }
}
impl<const D: usize, const R: usize> Iterator for MoveIter<D, R>
    where [(); 3+R*2]:
{
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        'main: loop {
            // eprintln!("entering MoveIter::next loop with {:?}", self.next);
            match self.next.clone() {
                MoveIterState::Done => return None,
                MoveIterState::RoomToHallway { next_room, src, mut range, src_depth, src_hall_slot, pod } => {
                    // note that `range` is borrowed mutably so checking this changes self.next
                    
                    // range is mostly pre-validated - just yield it until exhausted
                    while let Some(tgt) = range.next() {
                        if tgt >= 2 && (tgt as usize) < 2+R*2 && tgt % 2 == 0 {
                            // cannot stop directly outside rooms
                            continue;
                        }

                        self.next = MoveIterState::RoomToHallway {
                            next_room,
                            src,
                            src_depth,
                            src_hall_slot,
                            pod,
                            range,
                        };
                        return Some(Move {
                            src, // a Position::Room
                            dst: Position::Hallway(tgt),
                            len: src_depth+1 + tgt.abs_diff(src_hall_slot),
                            pod,
                        });
                    }

                    // exhausted the range
                    self.next = MoveIterState::Room { room: next_room, depth: 0 };
                }
                MoveIterState::Hallway(i) => {
                    if i as usize >= self.state.hall.len() {
                        debug_assert_eq!(self.state.hall.len(), i as usize);
                        self.next = MoveIterState::Room { room: 0, depth: 0 };
                        continue;
                    }

                    // scan over the hallway looking for amphipods that need a home
                    for i in i..(self.state.hall.len() as u8) {
                        if let Some(p) = self.state.hall[i as usize] {
                            // we found a pod, can it go home?

                            // is it's home room available?
                            match self.state.test_room(p) {
                                RoomStatus::Available(dst) => {
                                    // room is available - is there a path to it?
                                    // we are currently in the hallway, and we know there is room in our pod
                                    // so we only have to test from ourselves to the pod's entrace
                                    let entrance = p.entrance();
                                    
                                    // get the length through the hallway while we test if it's available
                                    let hall_len: u8 = if i as u8 == entrance {
                                        0 // we are already at our entrance
                                    } else {
                                        match self.state.path_available(
                                            Position::Hallway(i as u8),
                                            Position::Hallway(entrance),
                                        ) {
                                            None => {
                                                // no path available - skip this
                                                continue;
                                            },
                                            Some(len) => {
                                                // there is a path available through the hallway, use it
                                                len+1
                                            },
                                        }
                                    };

                                    self.next = MoveIterState::Hallway((i+1) as u8);
                                    return Some(Move {
                                        src: Position::Hallway(i as u8),
                                        dst: Position::Room { pod: p, depth: dst },
                                        len: (hall_len) + dst,
                                        pod: p,
                                    });
                                },

                                // room has foreigners - ignore it
                                // pods can't move across a hallway anyway
                                RoomStatus::Occupied => continue,

                                // the room was found to be full, but we have one trying to go home
                                // this is a programming error
                                RoomStatus::Full if cfg!(debug_assertions) => {
                                    panic!("room for {:?} was found full, but there exists a pod for it:\n{}", p, self.state);
                                },
                                RoomStatus::Full => unsafe { std::hint::unreachable_unchecked() },
                            }
                        }
                    }

                    // no pods in hallway found - continue on to the rooms
                    self.next = MoveIterState::Room { room: 0, depth: 0, };
                },
                MoveIterState::Room { room, depth } => {
                    // test the top of each room - if it does not belong in that room, try to move it
                    'rooms: for ri in (room as usize)..R {
                        let intended_pod = Pod::try_from_u8(ri as u8).unwrap();
                        for di in (depth as usize)..D {
                            let cell = self.state.rooms[ri][di];

                            // this cell is empty - keep going deeper
                            let occupant = match cell {
                                None => continue,
                                Some(p) => p,
                            };

                            if occupant == intended_pod {
                                // this is us - do we need to move for another pod?
                                let lower_pods_correct = self.state.rooms[ri][di as usize+1..D].iter()
                                    .map(|op| op.expect("found empty cell below active cell - this is an illegal state"))
                                    .all(|p| p == intended_pod);
                                
                                if lower_pods_correct {
                                    // this room has all the correct pod type (including us) - skip this room
                                    continue 'rooms;
                                }
                            }

                            // we are a foreigner that needs to move, or need to move for another to leave
                            // eprintln!("at {:?} - we are ourselves a foreigner - {:?}", Position::Room { pod: intended_pod, depth: di as u8, }, cell);

                            // can target our destination room, or a spot in the hallway
                            // - where in the hallway can we go? what is left/right of our entrance?
                            let room_entrance = intended_pod.entrance();
                            
                            // Scanning backwords from `room_entrance`, find the first occupied slot.
                            let hallway_start_opt = self.state.hall.iter()
                                .enumerate()
                                .take(room_entrance as usize).rev()
                                .filter_map(|(i, &op)| op.map(|p| (i, p)))
                                .next();

                            // Scanning forwards from `room_entrance`, find the first occupied slot.
                            let hallway_end_opt = self.state.hall.iter()
                                .enumerate()
                                .skip(room_entrance as usize)
                                .filter_map(|(i, &op)| op.map(|p| (i, p)))
                                .next();

                            // get the range of available hallway targets
                            let (range_lower, range_upper) = match (hallway_start_opt, hallway_end_opt) {
                                (Some((si, _)), Some((ei, _))) => (si+1, ei),
                                (Some((si, _)), None) => (si+1, self.state.hall.len()),
                                (None, Some((ei, _))) => (0, ei),
                                (None, None) => (0, self.state.hall.len()),
                            };
                            let range = (range_lower as u8)..(range_upper as u8);

                            // if we are not in our own pod entrance
                            if let Some(bad_occupant) = cell.filter(|&p| p != intended_pod) {
                                // if we can path into our own room
                                let tgt_entrance = bad_occupant.entrance();
                                if range.contains(&(tgt_entrance as u8)) {
                                    // if our room is actually available
                                    match self.state.test_room(bad_occupant) {
                                        RoomStatus::Occupied => {
                                            // our intended room is unavailable - can only go into hallway
                                            // eprintln!("our room is unavailable - try hall");
                                        },
                                        RoomStatus::Available(i) => {
                                            // our intended room is available - go into it
                                            let len = di as u8 + i + 2 + room_entrance.abs_diff(tgt_entrance);

                                            // shift to hallway scanning - it could be beneficial to go to hallway
                                            // instead of directly to a room
                                            self.next = MoveIterState::RoomToHallway {
                                                next_room: (ri as u8) + 1,
                                                src_depth: di as u8,
                                                src_hall_slot: room_entrance as u8,
                                                src: Position::Room { pod: intended_pod, depth: di as u8, },
                                                range,
                                                pod: bad_occupant,
                                            };
                                            // self.next = MoveIterState::Room { room: (ri+1) as u8, depth: 0 };
                                            
                                            return Some(Move {
                                                src: Position::Room { pod: intended_pod, depth: di as u8 },
                                                dst: Position::Room { pod: bad_occupant, depth: i },
                                                len: len as u8,
                                                pod: bad_occupant,
                                            });
                                        },
                                        // the room was found to be full, but we have one trying to go home
                                        // this is a programming error
                                        RoomStatus::Full if cfg!(debug_assertions) => {
                                            panic!("room for {:?} was found full, but there exists a pod for it:\n{}", bad_occupant, self.state);
                                        },
                                        RoomStatus::Full => unsafe { std::hint::unreachable_unchecked() },
                                    }
                                }
                            }

                            // eprintln!("shifting states to hallway");
                            // we are unable to move into our own room - try the hallway
                            self.next = MoveIterState::RoomToHallway {
                                next_room: (ri as u8) + 1,
                                src_depth: di as u8,
                                src_hall_slot: room_entrance as u8,
                                src: Position::Room { pod: intended_pod, depth: di as u8, },
                                range,
                                pod: occupant,
                            };
                            continue 'main;
                        }
                    }

                    self.next = MoveIterState::Done;
                },
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Position {
    Hallway(u8),
    Room { pod: Pod, depth: u8 },
}

#[derive(Debug, Clone, Copy)]
pub struct Move {
    src: Position,
    dst: Position,
    len: u8,
    pod: Pod,
}
impl Move {
    pub fn src(&self) -> Position { self.src }
    pub fn dst(&self) -> Position { self.dst }
    pub fn len(&self) -> usize { self.len as usize }
    pub fn pod(&self) -> Pod { self.pod }


    /// In many cases, the length can be computed using a sum of components when trying to find an available path
    /// 
    /// This check re-computes it in a more normalized way to ensure all the intermediate lengths are correct
    #[track_caller]
    pub fn assert_length(&self) {
        fn room_and_hallway(hall: u8, room: Pod, depth: u8) -> u8 {
            let dst_pod_entrance = room.entrance();
            let hall_len = hall.abs_diff(dst_pod_entrance);
            depth + hall_len + 1
        }

        if cfg!(debug_assertions) {
            let expected = match (self.src, self.dst) {
                (Position::Hallway(_), Position::Hallway(_)) => panic!("cannot move within the hallway. attempted move from {:?} to {:?}", self.src, self.dst),
                (Position::Room { pod: spod, depth: sdepth, }, Position::Room { pod: dpod, depth: ddepth, }) => {
                    if spod == dpod {
                        panic!("attempt to move pod within the same room. this should have no reason to occur.");
                    }
                    let src_pod_entrnace = spod.entrance();
                    let dst_pod_entrance = dpod.entrance();
                    let hall_len = src_pod_entrnace.abs_diff(dst_pod_entrance);
                    sdepth + ddepth + hall_len + 2
                },
                (Position::Hallway(hall), Position::Room { pod, depth }) => room_and_hallway(hall, pod, depth),
                (Position::Room { pod, depth }, Position::Hallway(hall)) => room_and_hallway(hall, pod, depth),
            };

            if expected != self.len {
                panic!("expected ({}) and cached ({}) lengths not equal for move from {:?} to {:?}!", expected, self.len, self.src, self.dst);
            }
        }
    }
}

pub enum RoomStatus {
    Available(u8),
    Full,
    Occupied,
}
impl RoomStatus {
    pub fn test<const D: usize>(room: [Option<Pod>; D], p: Pod) -> RoomStatus {
        // does it contain foreigners?
        if room.iter().flatten().any(|&rp| rp != p) {
            return RoomStatus::Occupied;
        }

        // does it contain empty spaces below proper pods?
        // -- pods need to move down before anything else
        if cfg!(debug_assertions) {
            for i in 1..room.len() {
                let (upper, lower) = (room[i-1], room[i]);
                if upper.is_some() && lower.is_none() {
                    panic!("found an uncompressed room - this is an error");
                }
            }
        }

        // room likely available - find next spot
        for i in (0..room.len()).rev() {
            if room[i].is_none() {
                return RoomStatus::Available(i as u8);
            }
        }
        
        RoomStatus::Full
    }
}

pub struct Solutions<const D: usize, const R: usize>
    where [(); 3+R*2]:
{
	stack: Vec<(MoveIter<D, R>, MoveCount)>,
	pub found: HashMap<GameState2<D, R>, MoveCount>,
}
impl<const D: usize, const R: usize> Solutions<D, R>
    where [(); 3+R*2]:
{
	fn new(game: GameState2<D, R>) -> Self {
		let mut found = HashMap::new();
		found.insert(game, MoveCount::ZERO);

		Solutions {
			stack: vec![
				(game.movable(), MoveCount::ZERO),
			],
			found,
		}
	}
}
impl<const D: usize, const R: usize> Iterator for Solutions<D, R>
    where [(); 3+R*2]:
{
	type Item = MoveCount;
	fn next(&mut self) -> Option<Self::Item> {
		while let Some((mut pmoves, mcount)) = self.stack.pop() {
			if let Some(mov) = pmoves.next() {
				// put it back - there could be another one after this
				self.stack.push((pmoves.clone(), mcount));
                mov.assert_length();

				// add a new possible game state to the stack
				let new_game = pmoves.state.play(&mov);
				let new_score = mcount + mov;

				// eprintln!("normalized, pushing");

				if new_game.has_won() {
					// this is a winning state - return it
					return Some(new_score);
				} else {
					if let Some(found_score) = self.found.get(&new_game) {
						if *found_score < new_score {
							// if DEBUG_OUT && new_score.sum() - found_score.sum() > 10000 {
								// eprintln!("found previous better score for same state (cached {:?}, had {:?}) -- (cached {}, had {})", found_score, new_score, found_score.sum(), new_score.sum());
							// }
							continue;
						}
					}

					// if DEBUG_OUT {
					// 	eprintln!("moving forward on path: ({:?}[{} -> {}], {} tiles) ({} unique states found)", ua, pmoves.state[ua], dst, moved, self.found.len());
					// 	for (i, (new, old)) in new_game.to_string().lines().zip(pmoves.state.to_string().lines()).enumerate() {
					// 		eprintln!("{:<13} {:<3} {:<13}", old, if i == 2 { "-->" } else { "" }, new);
					// 	}
					// }

					// only try it if we haven't been in this state yet - or this is lower than one we've previously found
					self.found.insert(new_game, new_score);
					self.stack.push((
						new_game.movable(),
						new_score,
					));
				}
			}
		}

		None
	}
}

#[cfg(test)]
const EXAMPLES_2: &str = include_str!("../example_2.txt");
#[cfg(test)]
const EXAMPLES_4: &str = include_str!("../example_4.txt");

#[test]
fn parsing() {
    fn _parsing<const D: usize>(samples: &str) {
        for (i, case) in samples.split("\n\n").map(str::trim).enumerate() {
            let parsed = GameState2::<D, 4>::try_parse(case)
                .unwrap_or_else(|_| panic!("failed parsing example #{}", i));
            
            let rendered = parsed.to_string();
    
            assert_eq!(case, rendered, "example #{} - rendered is not equilavent to parsed. parsed:\n{}", i, parsed);
        }
    }
    _parsing::<2>(EXAMPLES_2);
    _parsing::<4>(EXAMPLES_4);
}

#[test]
fn sample_moves_exist() {
    fn _sample_moves_exist<const D: usize>(samples: &str, score: usize) {
        let parsed: Vec<GameState2<D, 4>> = samples.split("\n\n")
            .map(str::trim)
            .map(GameState2::try_parse)
            .map(Result::unwrap)
            .collect();

        let mut moves: Vec<Move> = Vec::new();

        for i in 0..parsed.len()-1 {
            let (start, end) = (parsed[i], parsed[i+1]);
            moves.push(start.movable()
                // .inspect(|m| eprintln!("move: {:?}", m))
                .inspect(|m| m.assert_length())
                .find(|mov| start.play(&mov) == end)
                .unwrap_or_else(|| {
                    panic!("unable to find move for iteration from example state {} to {}\n{}\nto\n{}", i, i+1, start, end)
                }));

            assert!(!start.has_won(), "non-winning state reported itself as won: \n{}", start);
            if i == parsed.len()-2 {
                assert!(end.has_won(), "winning state did not report itself as won: \n{}", end);
            }
        }

        // for m in &moves {
        //     eprintln!("final move: {:?}", m);
        // }
        let gen_score: MoveCount = moves.into_iter().collect();
        assert_eq!(score, gen_score.sum(), "unexpected score for correct moves (D={}): {:?}", D, gen_score);
    }

    // A=8, B=11, C=4, D=10

    _sample_moves_exist::<2>(EXAMPLES_2, 12521);
    _sample_moves_exist::<4>(EXAMPLES_4, 44169);
}

