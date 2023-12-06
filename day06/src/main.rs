use std::fs::File;
use std::io::{self, BufRead, Error};

fn parse_nums(line: &str) -> Option<Vec<i64>> {
    line.split_once(':')
        .map(|(_label, nums)| {
            nums.split_whitespace()
                .filter_map(|x| x.parse().ok())
                .collect()
        })
}

type Input = (Vec<i64>, Vec<i64>);

fn parse_part1(mut source: impl Iterator<Item = String>) -> Option<Input> {
    let times = parse_nums(&source.next()?)?;
    let distances = parse_nums(&source.next()?)?;
    Some((times, distances))
}

fn race(time: i64, distance: i64) -> usize {
    (1..time)
        .filter(|speed| speed * (time - speed) > distance)
        .count()
}

fn part1(times: Vec<i64>, distances: Vec<i64>) -> usize {
    times.iter().zip(distances.iter())
        .map(|(&time, &dist)| race(time,dist))
        .product()
}

fn parse_num(line: &str) -> Option<i64> {
    line.split_once(':')
        .map(|(_label, nums)| {
            nums.split_whitespace()
                .collect::<String>()
                .parse().ok()
        }).flatten()
}

fn parse_part2(mut source: impl Iterator<Item = String>) -> Option<(i64, i64)> {
    let time = parse_num(&source.next()?)?;
    let distance = parse_num(&source.next()?)?;
    Some((time, distance))
}

fn part2(time: i64, distance: i64) -> usize {
    race(time, distance)
}

macro_rules! do_part {
    ($parse:ident, $part:ident) => {
        {
            let file = File::open("input.txt")?;
            let reader = io::BufReader::new(file);
            let lines = reader.lines().map(|l| l.unwrap());

            let (times, distances) =
                $parse(lines).ok_or(Error::other(format!("{}: error", stringify!($parse))))?;
            println!("{}: {}", stringify!($part), $part(times, distances));
        }
    };
}

fn main() -> io::Result<()> {
    do_part!(parse_part1, part1);
    do_part!(parse_part2, part2);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn test_data() -> Vec<String> {
        vec![
            "Time:      7  15   30".to_string(),
            "Distance:  9  40  200".to_string(),
        ]
    }

    #[test]
    fn test_parse_part1() {
        let expected_result = Some((vec![7, 15, 30], vec![9, 40, 200]));
        assert_eq!(parse_part1(test_data().into_iter()), expected_result);
    }

    #[test]
    fn test_part1() {
        if let Some((times, distances)) = parse_part1(test_data().into_iter()) {
            assert_eq!(part1(times, distances), 288);
        }
    }

    #[test]
    fn test_parse_part2() {
        let expected_result = Some((71530, 940200));
        assert_eq!(parse_part2(test_data().into_iter()), expected_result);
    }

    #[test]
    fn test_part2() {
        if let Some((times, distances)) = parse_part2(test_data().into_iter()) {
            assert_eq!(part2(times, distances), 71503);
        }
    }
}
