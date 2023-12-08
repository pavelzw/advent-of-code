mod y2023;

use std::os::unix::prelude::OsStrExt;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{fmt, fs, io};

use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(required = true)]
    files: Vec<PathBuf>,
}

macro_rules! days {
    ( $( $day:ident : $name:expr => $path:path, )* ) => {
        enum Day { $( $day, )* }

        impl fmt::Display for Day {
            fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $( Self::$day => $name, )*
                }.fmt(f)
            }
        }

        impl FromStr for Day {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(match s {
                    $( $name => Self::$day, )*
                    _ => return Err(()),
                })
            }
        }

        impl Day {
            fn solve(self, input: String) {
                match self {
                    $( Self::$day => $path(input), )*
                }
            }
        }
    };
}

days! {
    Y2023D01: "01" => y2023::d01::solve,
    Y2023D02: "02" => y2023::d02::solve,
    Y2023D03: "03" => y2023::d03::solve,
    Y2023D04: "04" => y2023::d04::solve,
    Y2023D05: "05" => y2023::d05::solve,
    Y2023D06: "06" => y2023::d06::solve,
    Y2023D07: "07" => y2023::d07::solve,
    Y2023D08: "08" => y2023::d08::solve,
}

impl Day {
    fn from_path(path: &Path) -> Option<Self> {
        let file_name = path.file_stem()?.as_bytes();
        let file_name = std::str::from_utf8(file_name).ok()?;
        let day = file_name.strip_prefix("Day")?;
        let day = day.parse().ok()?;
        Some(day)
    }
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    for file in args.files {
        let day = match Day::from_path(&file) {
            Some(day) => day,
            None => {
                eprintln!("Could not parse day from file {:?}", file);
                continue;
            }
        };

        eprintln!("Solving day {}", day);
        let input = fs::read_to_string(file)?;
        day.solve(input);
        eprintln!();
    }
    Ok(())
}
