fn extrapolate(sequence: Vec<i32>) -> i32 {
    if sequence.iter().all(|&n| n == 0) {
        return 0;
    }
    let differences = sequence.windows(2).map(|w| w[1] - w[0]).collect::<Vec<_>>();
    sequence.last().unwrap() + extrapolate(differences)
}

pub fn solve(input: String) {
    let input = input
        .split('\n')
        .filter(|l| !l.is_empty())
        .map(|l| {
            l.split(' ')
                .map(|w| w.parse::<i32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let solution_1 = input.iter().map(|l| extrapolate(l.clone())).sum::<i32>();
    println!("Part 1: {}", solution_1);
    let solution_2 = input
        .iter()
        .map(|l| {
            let mut l = l.clone();
            l.reverse();
            extrapolate(l)
        })
        .sum::<i32>();
    println!("Part 2: {}", solution_2);
}
