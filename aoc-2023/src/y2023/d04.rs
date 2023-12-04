struct Card {
    winning_numbers: Vec<u8>,
    numbers: Vec<u8>,
}

fn num_winning(card: &Card) -> usize {
    card.numbers
        .iter()
        .filter(|number| card.winning_numbers.contains(number))
        .count()
}

pub fn solve(input: String) {
    let cards = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let (winning_numbers, numbers) = line.split_once('|').unwrap();
            let winning_numbers = winning_numbers
                .split_once(':')
                .unwrap()
                .1
                .trim()
                .split(' ')
                .filter_map(|number| number.parse::<u8>().ok())
                .collect::<Vec<_>>();
            let numbers = numbers
                .trim()
                .split(' ')
                .filter_map(|number| number.parse::<u8>().ok())
                .collect::<Vec<_>>();
            Card {
                winning_numbers,
                numbers,
            }
        })
        .collect::<Vec<_>>();

    let solution_1 = cards
        .iter()
        .map(|card| {
            let num_winning = num_winning(card);
            if num_winning == 0 {
                0
            } else {
                2_u32.pow((num_winning - 1) as u32)
            }
        })
        .sum::<u32>();
    println!("Level 1: {}", solution_1);

    let mut copies = vec![1; cards.len()];
    for (i, card) in cards.iter().enumerate() {
        let num_winning = num_winning(card);
        for j in i + 1..=i + num_winning {
            copies[j] += copies[i];
        }
    }
    let solution_2 = copies.iter().sum::<u32>();
    println!("Level 2: {}", solution_2);
}
