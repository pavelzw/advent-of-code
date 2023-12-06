fn get_num_possibilities_to_reach_distance(time: usize, distance: usize) -> usize {
    // d(time_pressed) = time_pressed * (time - time_pressed) = time_pressed * time - time_pressed^2
    // search for d(time_pressed) > distance
    // time_pressed^2 - time_pressed * time + distance < 0
    // time_pressed = (time +- sqrt(time^2 - 4 * distance)) / 2
    let time = time as f64;
    let distance = distance as f64 + 1e-6; // add epsilon since we want ">" and not ">="
    let time_pressed_0 = (time - (time * time - 4. * distance).sqrt()) / 2.;
    let time_pressed_1 = (time + (time * time - 4. * distance).sqrt()) / 2.;
    let time_pressed_0 = time_pressed_0.ceil() as usize;
    let time_pressed_1 = time_pressed_1.floor() as usize;
    (time_pressed_1 - time_pressed_0) + 1
}

pub fn solve(input: String) {
    let lines = input
        .split('\n')
        .filter(|l| !l.is_empty())
        .collect::<Vec<&str>>();
    let times = lines
        .first()
        .unwrap()
        .strip_prefix("Time:")
        .unwrap()
        .trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    let distances = lines
        .get(1)
        .unwrap()
        .strip_prefix("Distance:")
        .unwrap()
        .trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    let solution_1 = times
        .iter()
        .zip(distances)
        .map(|(&t, d)| get_num_possibilities_to_reach_distance(t, d))
        .product::<usize>();
    println!("Level 1: {}", solution_1);

    let times = lines
        .first()
        .unwrap()
        .strip_prefix("Time:")
        .unwrap()
        .chars()
        .filter(|&c| c != ' ')
        .collect::<String>()
        .parse::<usize>()
        .unwrap();
    let distances = lines
        .get(1)
        .unwrap()
        .strip_prefix("Distance:")
        .unwrap()
        .chars()
        .filter(|&c| c != ' ')
        .collect::<String>()
        .parse::<usize>()
        .unwrap();
    let solution_2 = get_num_possibilities_to_reach_distance(times, distances);
    println!("Level 2: {}", solution_2);
}
