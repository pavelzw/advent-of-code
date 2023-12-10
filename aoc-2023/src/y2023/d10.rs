#[derive(Eq, PartialEq)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn opposite(&self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::East => Direction::West,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum Tile {
    PipeVertical,
    PipeHorizontal,
    PipeNorthEast,
    PipeNorthWest,
    PipeSouthEast,
    PipeSouthWest,
    Ground,
    Start,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
struct Coord {
    // row 0 = up north
    // col 0 = left west
    row: usize,
    col: usize,
}

impl Coord {
    fn go(&self, direction: Direction) -> Self {
        match direction {
            Direction::North => Coord {
                row: self.row - 1,
                col: self.col,
            },
            Direction::East => Coord {
                row: self.row,
                col: self.col + 1,
            },
            Direction::South => Coord {
                row: self.row + 1,
                col: self.col,
            },
            Direction::West => Coord {
                row: self.row,
                col: self.col - 1,
            },
        }
    }
}

fn tile_from_char(c: char) -> Tile {
    match c {
        '|' => Tile::PipeVertical,
        '-' => Tile::PipeHorizontal,
        'L' => Tile::PipeNorthEast,
        'J' => Tile::PipeNorthWest,
        '7' => Tile::PipeSouthWest,
        'F' => Tile::PipeSouthEast,
        '.' => Tile::Ground,
        'S' => Tile::Start,
        _ => panic!("Invalid card: {}", c),
    }
}

fn next_direction(grid: &[Vec<Tile>], coord: &Coord, coming_from: &Direction) -> Direction {
    let tile = &grid[coord.row][coord.col];
    match coming_from {
        Direction::North => match tile {
            Tile::PipeVertical => Direction::South,
            Tile::PipeNorthWest => Direction::West,
            Tile::PipeNorthEast => Direction::East,
            _ => panic!("Invalid tile at coord {:?}: {:?}", coord, tile),
        },
        Direction::South => match tile {
            Tile::PipeVertical => Direction::North,
            Tile::PipeSouthEast => Direction::East,
            Tile::PipeSouthWest => Direction::West,
            _ => panic!("Invalid tile at coord {:?}: {:?}", coord, tile),
        },
        Direction::East => match tile {
            Tile::PipeHorizontal => Direction::West,
            Tile::PipeNorthEast => Direction::North,
            Tile::PipeSouthEast => Direction::South,
            _ => panic!("Invalid tile at coord {:?}: {:?}", coord, tile),
        },
        Direction::West => match tile {
            Tile::PipeHorizontal => Direction::East,
            Tile::PipeNorthWest => Direction::North,
            Tile::PipeSouthWest => Direction::South,
            _ => panic!("Invalid tile at coord {:?}: {:?}", coord, tile),
        },
    }
}

pub fn solve(input: String) {
    let grid = input
        .split('\n')
        .filter(|l| !l.is_empty())
        .map(|l| l.chars().map(tile_from_char).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let coord_start = grid
        .iter()
        .enumerate()
        .find_map(|(y, row)| {
            row.iter().enumerate().find_map(|(x, tile)| {
                if *tile == Tile::Start {
                    Some(Coord { row: y, col: x })
                } else {
                    None
                }
            })
        })
        .unwrap();
    let mut direction = Direction::South; // TODO don't hardcode this
    let mut coming_from = direction.opposite();
    let mut coord = coord_start.go(direction);
    let mut num_steps = 1;
    while grid[coord.row][coord.col] != Tile::Start {
        direction = next_direction(&grid, &coord, &coming_from);
        coming_from = direction.opposite();
        coord = coord.go(direction);
        num_steps += 1;
    }
    let solution_1 = num_steps / 2;

    println!("Part 1: {}", solution_1);
}
