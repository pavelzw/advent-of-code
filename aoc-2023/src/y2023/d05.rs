struct MapEntry {
    dest_start: usize,
    src_start: usize,
    length: usize,
}
struct Map {
    entries: Vec<MapEntry>,
}
impl Map {
    fn apply(&self, input: usize) -> usize {
        let mut output = input; // default case
        for entry in self.entries.iter() {
            if input >= entry.src_start && input < entry.src_start + entry.length {
                output = entry.dest_start + (output - entry.src_start);
            }
        }
        output
    }
}

pub fn solve(input: String) {
    let lines = input.split("\n").filter(|l| !l.is_empty()).collect::<Vec<&str>>();
    let seeds = lines[0].split_once(": ").unwrap().1;
    let seeds = seeds.split(" ")
        .map(|s| s.parse::<usize>().unwrap()).collect::<Vec<_>>();
    let mut maps = Vec::new();
    for &line in lines[1..].iter() {
        let first = line.chars().next().unwrap();
        if first.is_alphabetic() {
            let map = Map { entries: Vec::new() };
            maps.push(map);
        } else {
            let map = maps.last_mut().unwrap();
            let mut parts = line.split(" ");
            let dest_start = parts.next().unwrap().parse::<usize>().unwrap();
            let src_start = parts.next().unwrap().parse::<usize>().unwrap();
            let length = parts.next().unwrap().parse::<usize>().unwrap();
            map.entries.push(MapEntry { dest_start, src_start, length });
        }
    }

    let solution_1 = seeds.iter().map(|&s| {
        let mut location = s;
        for map in maps.iter() {
            location = map.apply(location);
        }
        location
    }).min().unwrap();
    println!("Solution 1: {:?}", solution_1);
}
