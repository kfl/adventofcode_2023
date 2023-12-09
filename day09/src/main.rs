use std::fs::File;
use std::io::{self, BufRead};
use std::iter::successors;

fn read_lines(filename: &str) -> io::Result<impl Iterator<Item = String>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let lines = reader
        .lines()
        .map(|line| line.expect("Could not read lines"));
    Ok(lines)
}

fn parse(source: impl Iterator<Item = String>) -> Vec<Vec<i64>> {
    source
        .map(|s| {
            s.split_whitespace()
                .filter_map(|x| x.parse().ok())
                .collect()
        })
        .collect()
}

fn diffs(xs: &[i64]) -> Vec<i64> {
    xs.windows(2).map(|w| w[1] - w[0]).collect()
}

fn full_history(xs: &[i64]) -> impl Iterator<Item = Vec<i64>> {
    successors(Some(xs.to_vec()), |prev| {
        let next = diffs(prev);
        next.iter().any(|&x| x != 0).then_some(next)
    })
}

fn part1(input: &[&[i64]]) -> i64 {
    input
        .iter()
        .map(|&v| {
            full_history(v)
                .map(|inner| *inner.last().unwrap())
                .sum::<i64>()
        })
        .sum()
}

fn diff(xs: &[i64]) -> i64 {
    let (last, rest) = xs.split_last().unwrap();
    rest.iter().rfold(*last, |x, &acc| acc - x)
}

fn part2(input: &[&[i64]]) -> i64 {
    input.iter()
        .map(|&v| {
            diff(
                &full_history(v)
                    .map(|inner| *inner.first().unwrap())
                    .collect::<Vec<_>>(),
            )
        })
        .sum()
}

fn main() -> io::Result<()> {
    let lines = read_lines("input.txt")?;
    let input = parse(lines);
    let input_slices: Vec<&[i64]> = input.iter().map(|v| v.as_slice()).collect();
    println!("Part 1: {}", part1(&input_slices));
    println!("Part 2: {}", part2(&input_slices));
    Ok(())
}
