use num::integer::lcm;
use std::collections::HashMap;

#[derive(Copy, Clone)]
enum Direction {
    Left,
    Right,
}
fn direction_from_char(c: char) -> Direction {
    match c {
        'L' => Direction::Left,
        'R' => Direction::Right,
        _ => panic!("Invalid direction char: {}", c),
    }
}

pub fn solve(input: String) {
    let input = input
        .split('\n')
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();
    let directions = input[0]
        .chars()
        .map(direction_from_char)
        .collect::<Vec<_>>();
    let mut network = HashMap::new();
    input.iter().skip(1).for_each(|&l| {
        // get first three chars
        let start = &l[0..3];
        let left_next = &l[7..10];
        let right_next = &l[12..15];
        network.insert(start, (left_next, right_next));
    });

    let mut pos = "AAA";
    let mut steps = 0;
    while pos != "ZZZ" {
        let (left, right) = network.get(pos).unwrap();
        let dir = directions[steps % directions.len()];
        pos = match dir {
            Direction::Left => left,
            Direction::Right => right,
        };
        steps += 1;
    }
    println!("Part 1: {}", steps);

    // every node ending with A has only one goal (ending with Z)
    // -> compute least common multiple
    let start_positions = network
        .iter()
        .filter(|(&k, _)| k.ends_with('A'))
        .map(|(&k, _)| k);
    let steps = start_positions.map(|mut pos| {
        let mut steps = 0;
        while !pos.ends_with('Z') {
            let (left, right) = network.get(pos).unwrap();
            pos = match directions[steps % directions.len()] {
                Direction::Left => left,
                Direction::Right => right,
            };
            steps += 1;
        }
        steps
    });
    let lcm = steps.fold(1_usize, lcm);
    println!("Part 2: {}", lcm);
}
