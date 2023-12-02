use std::collections::HashMap;

struct Config {
    red: i32,
    green: i32,
    blue: i32,
}
struct Game {
    id: i32,
    configs: Vec<Config>,
}
pub fn solve(input: String) {
    let games = input.lines().filter(|line| !line.is_empty()).map(|line| {
        let (game_id, description) = line.split_once(':').unwrap();
        let (_, game_id) = game_id.split_once(' ').unwrap();
        let game_id = game_id.parse::<i32>().unwrap();
        let configs = description
            .split(';')
            .map(|config| {
                let config = config.trim();
                let values: HashMap<_, _> = config
                    .split(',')
                    .map(|value| {
                        let (value, name) = value.trim().split_once(' ').unwrap();
                        let value = value.parse::<i32>().unwrap();
                        match name {
                            "red" | "green" | "blue" => (name, value),
                            _ => panic!("{} not found", name),
                        }
                    })
                    .collect();
                let red = *values.get("red").unwrap_or(&0);
                let green = *values.get("green").unwrap_or(&0);
                let blue = *values.get("blue").unwrap_or(&0);
                Config { red, green, blue }
            })
            .collect::<Vec<Config>>();
        Game {
            id: game_id,
            configs,
        }
    });
    let max_red = 12;
    let max_green = 13;
    let max_blue = 14;
    let solution_1 = games
        .clone()
        .filter(|game| {
            game.configs.iter().all(|config| {
                config.red <= max_red && config.green <= max_green && config.blue <= max_blue
            })
        })
        .map(|game| game.id)
        .sum::<i32>();
    let solution_2 = games
        .map(|game| {
            let red = game.configs.iter().map(|c| c.red).max().unwrap();
            let green = game.configs.iter().map(|c| c.green).max().unwrap();
            let blue = game.configs.iter().map(|c| c.blue).max().unwrap();
            let power = red * green * blue;
            power
        })
        .sum::<i32>();
    println!("Level 1: {}", solution_1);
    println!("Level 2: {}", solution_2);
}
