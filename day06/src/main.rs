use std::fs::File;
use std::io::{self, BufRead, Error};

fn read_lines(filename: &str) -> io::Result<impl Iterator<Item = String>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let lines = reader.lines().map(|line| line.expect("Could not read lines"));

    Ok(lines)
}

type Part1Input = (Vec<i64>, Vec<i64>);

fn parse_nums(line: &str) -> Option<Vec<i64>> {
    line.split_once(':')
        .map(|(_label, nums)| {
            nums.split_whitespace()
                .filter_map(|x| x.parse().ok())
                .collect()
        })
}

fn parse_part1(source: &[String]) -> Option<Part1Input> {
    let times = parse_nums(&source[0])?;
    let distances = parse_nums(&source[1])?;
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
        .and_then(|(_label, nums)| {
            nums.split_whitespace()
                .collect::<String>()
                .parse().ok()
        })
}

fn parse_part2(source:  &[String]) -> Option<(i64, i64)> {
    let time = parse_num(&source[0])?;
    let distance = parse_num(&source[1])?;
    Some((time, distance))
}

fn part2(time: i64, distance: i64) -> usize {
    race(time, distance)
}

fn main() -> io::Result<()> {
    let lines : Vec<_> = read_lines("input.txt")?.collect();
    let (times, distances) = parse_part1(&lines).ok_or(Error::other("Part 2 parse error"))?;
    println!("Part 1: {}", part1(times, distances));
    let (time, distance) = parse_part2(&lines).ok_or(Error::other("Part 2 parse error"))?;
    println!("Part 2: {}", part2(time, distance));
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
        assert_eq!(parse_part1(&test_data()), expected_result);
    }

    #[test]
    fn test_part1() {
        let (times, distances) = parse_part1(&test_data()).expect("parse error");
        assert_eq!(part1(times, distances), 288);
    }

    #[test]
    fn test_parse_part2() {
        let expected_result = Some((71530, 940200));
        assert_eq!(parse_part2(&test_data()), expected_result);
    }

    #[test]
    fn test_part2() {
        let (time, distance) = parse_part2(&test_data()).expect("parse error");
        assert_eq!(part2(time, distance), 71503);
    }
}
