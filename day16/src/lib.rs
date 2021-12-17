#![allow(unused_imports)]
use std::io::{Cursor, Seek};
use std::str::FromStr;
use bitstream_io::{BitReader, BE, BitRead};
use itertools::Itertools;
use aoch::{AoCDay, DayResult};
use aoch::DayError;
#[cfg(test)] #[allow(unused_imports)]
use aoch::{DayPart, run_test, test_runner, daystr};

#[derive(Debug, Clone, Copy)]
enum PacketLength {
	BitLength(u16),
	SubPacketCount(u16),
}

#[derive(Debug, Clone)]
enum PacketData {
	Literal(u64),
	Container {
		typ_id: u8,
		_length: PacketLength,
		subpkts: Vec<Packet>
	},
}

#[derive(Debug, Clone)]
struct Packet {
	version: u8,
	data: PacketData,
}
impl Packet {
	fn parse(raw: &[u8]) -> Vec<Packet> {
		eprintln!("starting buffer read of {} bits", raw.len()*8);
		let mut c = Cursor::new(raw);
		let mut br = BitReader::<_, BE>::new(&mut c);
		let mut pkts = Vec::new();
		let mut bits_consumed = 0;
		while let Some((bitlen, pkt)) = Packet::from_reader(bits_consumed, &mut br) {
			bits_consumed += bitlen;
			eprintln!("consumed {}/{} bits of buffer (this={} bits) :: {:#?}", bits_consumed, raw.len()*8, bitlen, pkt);
			pkts.push(pkt);
			if raw.len()*8 - bits_consumed < 8 {
				break;
			}
		}
		assert_eq!(raw.len(), c.position() as usize, "did not consume entire packet");
		pkts
	}
	fn from_reader(bit_idx: usize, br: &mut BitReader<&mut Cursor<&[u8]>, BE>) -> Option<(usize, Packet)> {
		let mut i = bit_idx;
		macro_rules! log_bits {
			($n: expr, $templ: literal $($arg:tt)*) => {
				eprintln!(concat!("[{}:{} -> {}] ", $templ), i, $n, i+$n $($arg)* );
				i += $n;
			};
			() => {
				eprintln!("[{} -> {}] finished packet (len {})", bit_idx, i, i-bit_idx);
			};
		}

		let version = br.read::<u8>(3).ok()?; log_bits!(3, "version: {}", version); // eprintln!("[{}:{}] version: {}", bit_idx, 3, version);
		let typ_id = br.read::<u8>(3).ok()?; log_bits!(3, "typ_id: {}", typ_id);

		if typ_id == 4 { // literal value encoded
			// hope a u64 is large enough for any size
			let mut val: u64 = 0;
			let mut chunks = 0;
			loop {
				let brk = !br.read_bit().ok()?;

				chunks += 1;

				assert!(chunks*4 <= std::mem::size_of_val(&val)*8, "attempt to read more bits than a u64 can handle");

				val = (val << 4) | br.read::<u64>(4).ok()?;

				if brk { break; }
			}

			log_bits!(chunks*5, "literal: {}", val);
			log_bits!();
			Some((i - bit_idx, Packet {
				version,
				data: PacketData::Literal(val),
			}))
		} else { // operator packet
			let (length, subpkts) = if !br.read_bit().ok()? {
				let bitlen = br.read::<u16>(15).ok()?;
				log_bits!(1+15, "operator: bit-length: {}", bitlen);

				let mut pkts = Vec::new();
				let mut consumed = 0;
				while let Some((len, pkt)) = Packet::from_reader(i, br) {
					log_bits!(len, "operator: consumed sub-packet: {:?}", pkt);
					consumed += len;
					pkts.push(pkt);

					if (bitlen as usize) < consumed { panic!("packet consumed more bytes than allowed (consumed {}, allowed {})", len, bitlen as usize - (consumed - len)); }
					if (bitlen as usize) == consumed { break; }
				}

				assert_eq!(bitlen as usize, consumed);

				(PacketLength::BitLength(bitlen), pkts)
			} else {
				let subpktcnt = br.read::<u16>(11).ok()?;
				log_bits!(1+11, "operator: packet count: {}", subpktcnt);

				let mut pkts = Vec::new();
				while pkts.len() < subpktcnt as usize {
					let (len, pkt) = Packet::from_reader(i, br)?;
					log_bits!(len, "operator: consumed sub-packet: {:?}", pkt);
					pkts.push(pkt);
				}
				
				assert_eq!(subpktcnt as usize, pkts.len());

				(PacketLength::SubPacketCount(subpktcnt), pkts)
			};
			
			log_bits!();
			Some((i - bit_idx, Packet {
				version,
				data: PacketData::Container {
					typ_id,
					_length: length,
					subpkts,
				}
			}))
		}
	}
	fn version_sum(&self) -> usize {
		let inner = match &self.data {
			PacketData::Literal(_) => 0,
			PacketData::Container { subpkts, .. } => {
				subpkts.iter()
					.map(Packet::version_sum)
					.sum()
			}
		};
		self.version as usize + inner
	}
	fn evaluate(&self) -> u64 {
		match &self.data {
			PacketData::Literal(d) => *d,
			PacketData::Container { typ_id, subpkts, .. } => {
				match *typ_id {
					0 => { // sum
						subpkts.iter()
							.map(Packet::evaluate)
							.sum()
					},
					1 => { // product
						subpkts.iter()
							.map(Packet::evaluate)
							.product()
					},
					2 => { // min
						subpkts.iter()
							.map(Packet::evaluate)
							.min().expect("min packet type had no sub-packets")
					},
					3 => { // max
						subpkts.iter()
							.map(Packet::evaluate)
							.max().expect("max packet type had no sub-packets")
					},
					5 => { // greater than
						assert_eq!(2, subpkts.len());
						(subpkts[0].evaluate() > subpkts[1].evaluate()) as u64
					},
					6 => { // less than
						assert_eq!(2, subpkts.len());
						(subpkts[0].evaluate() < subpkts[1].evaluate()) as u64
					},
					7 => { // equal
						assert_eq!(2, subpkts.len());
						(subpkts[0].evaluate() == subpkts[1].evaluate()) as u64
					},
					unk => panic!("unknown packet type: {}", unk),
				}
			} 
		}
	}
}

pub struct Day16 {
	raw_packets: Vec<u8>,
}
impl Day16 {
}

impl AoCDay for Day16 {
	type Answer = usize;

	fn day() -> u8 { 16 }
	fn name() -> &'static str { "Packet Decoder" }

	fn parse(input: &str) -> DayResult<Self> {
		let raw_packets = hex::decode(input.trim()).unwrap();
		Ok(Day16 { raw_packets })
	}
	fn part1(&mut self) -> DayResult<Self::Answer> {
		let packets = Packet::parse(self.raw_packets.as_slice());
		assert_eq!(1, packets.len());

		Ok(packets.iter()
			.map(Packet::version_sum)
			.sum())		
	}
	fn part2(&mut self) -> DayResult<Self::Answer> {
		let packets = Packet::parse(self.raw_packets.as_slice());
		assert_eq!(1, packets.len());

		Ok(packets[0].evaluate() as usize)
	}
}

#[test]
fn part1() {
	let cases = [
		("8A004A801A8002F478", 16),
		("620080001611562C8802118E34", 12),
		("C0015000016115A2E0802F182340", 23),
		("A0016C880162017C3686B18A3D4780", 31),
		(daystr!("16"), 986),
	];
	test_runner::<Day16, _>(DayPart::Part1, &cases);
}
#[test]
fn part2() {
	let cases = [
		("C200B40A82", 3),
		("04005AC33890", 54),
		("880086C3E88112", 7),
		("CE00C43D881120", 9),
		("D8005AC2A8F0", 1),
		("F600BC2D8F", 0),
		("9C005AC2F8F0", 0),
		("9C0141080250320F1802104A08", 1),
		(daystr!("16"), 18234816469452),
	];
	test_runner::<Day16, _>(DayPart::Part2, &cases);
}
