struct NumberCoord {
    row: usize,
    start: usize,
    end: usize,
}
struct SymbolCoord {
    row: usize,
    col: usize,
}
fn l_inf_distance(number: &NumberCoord, symbol: &SymbolCoord) -> i32 {
    let distance_row = ((number.row as i32) - (symbol.row as i32)).abs();
    let distance_col = if symbol.col < number.start {
        number.start - symbol.col
    } else if symbol.col > number.end {
        symbol.col - number.end
    } else {
        0
    } as i32;
    let l_inf_distance = distance_row.max(distance_col);
    l_inf_distance
}

pub fn solve(input: String) {
    let lines = input.lines().filter(|l| l.len() > 0).collect::<Vec<_>>();
    let digit_coords: Vec<_> = input
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| c.is_digit(10))
                .map(move |(col, _)| (row, col))
        })
        .collect::<Vec<_>>();
    let mut number_coords = Vec::new();
    let mut start = digit_coords[0];
    let mut end = start;
    for &(row, col) in digit_coords.iter().skip(1) {
        if row == start.0 && col == end.1 + 1 {
            end = (row, col);
        } else {
            let number = lines[start.0][start.1..=end.1]
                .to_string()
                .parse::<i32>()
                .unwrap();
            number_coords.push((
                NumberCoord {
                    row: start.0,
                    start: start.1,
                    end: end.1,
                },
                number,
            ));
            start = (row, col);
            end = (row, col);
        }
    }
    number_coords.push((
        NumberCoord {
            row: start.0,
            start: start.1,
            end: end.1,
        },
        lines[start.0][start.1..=end.1]
            .to_string()
            .parse::<i32>()
            .unwrap(),
    ));
    let symbol_coords = input
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| !c.is_digit(10) && c != &'.')
                .map(move |(col, c)| (SymbolCoord{row, col}, c))
        })
        .collect::<Vec<_>>();

    let solution_1 = number_coords
        .iter()
        .filter_map(|(number_coord, number)| {
            let l_inf_distances = symbol_coords.iter().map(|(symbol_coord, _)| {
                l_inf_distance(number_coord, symbol_coord)
            });
            if l_inf_distances.min().unwrap() <= 1 {
                Some(number)
            } else {
                None
            }
        })
        .sum::<i32>();
    println!("Solution 1: {}", solution_1);

    let solution_2 = symbol_coords
        .iter()
        .filter(|(_, symbol)| symbol == &'*')
        .filter_map(|(symbol_coord, _)| {
            // a gear is adjacent to exactly two numbers
            let adjacent_numbers = number_coords
                .iter()
                .filter_map(|(number_coord, number)| {
                    let dist = l_inf_distance(number_coord, symbol_coord);
                    if dist <= 1 {
                        Some(number)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            if adjacent_numbers.len() == 2 {
                Some(adjacent_numbers[0] * adjacent_numbers[1])
            } else {
                None
            }
        })
        .sum::<i32>();
    println!("Solution 2: {:?}", solution_2);
}
