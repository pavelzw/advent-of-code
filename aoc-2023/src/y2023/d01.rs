macro_rules! map_string_to_number {
    ($s:expr) => {
        match $s {
            "one" | "1" => 1,
            "two" | "2" => 2,
            "three" | "3" => 3,
            "four" | "4" => 4,
            "five" | "5" => 5,
            "six" | "6" => 6,
            "seven" | "7" => 7,
            "eight" | "8" => 8,
            "nine" | "9" => 9,
            _ => panic!("{} not found", $s),
        }
    };
}

pub fn solve(input: String) {
    let sum_level_1: i32 = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let first = line.chars().find(|c| c.is_numeric()).unwrap();
            let last = line.chars().rev().find(|c| c.is_numeric()).unwrap();
            format!("{}{}", first, last).parse::<i32>().unwrap()
        })
        .sum();

    let sum_level_2: i32 = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let numbers = [
                "one", "1", "two", "2", "three", "3", "four", "4", "five", "5", "six", "6",
                "seven", "7", "eight", "8", "nine", "9",
            ];
            let positions = numbers
                .iter()
                .map(|n| (line.find(n), line.rfind(n)))
                .enumerate()
                .filter(|(_, (i, _))| i.is_some())
                .map(|(index, (find, rfind))| (index, find.unwrap(), rfind.unwrap()));
            let first = positions.clone().min_by_key(|(_, i, _)| *i).unwrap().0;
            let first = map_string_to_number!(numbers[first]);
            let last = positions.max_by_key(|(_, _, i)| *i).unwrap().0;
            let last = map_string_to_number!(numbers[last]);
            first * 10 + last
        })
        .sum();

    println!("Level 1: {}", sum_level_1);
    println!("Level 2: {}", sum_level_2);
}
