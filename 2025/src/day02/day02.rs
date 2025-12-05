use std::fs;

const INPUT_PATH: &str = "src/day02/input.txt";

fn main() -> (u64, u64) {
    let raw_file = fs::read_to_string(INPUT_PATH).expect("Failed to read input file");
    let ranges = parse_ranges(&raw_file);
    let all_ids = individual_numbers(ranges);

    let part1 = part1(&all_ids);
    println!("Part 1: strings that are mirrored: {}", part1);
    let part2 = part2(&all_ids);
    println!("Part 2: strings that have repeating patterns: {}", part2);

    (part1, part2)
}

fn part1(ids: &[u64]) -> u64 {
    ids.iter().filter(|&&id| int_doubled(id)).sum()
}

fn part2(ids: &[u64]) -> u64 {
    ids.iter().filter(|&&id| int_repeats(id)).sum()
}

fn parse_ranges(raw_file: &str) -> Vec<(u64, u64)> {
    raw_file
        .split(",")
        .map(|range| {
            let (start, stop) = range.split_once("-").unwrap();
            (start.parse::<u64>().unwrap(), stop.parse::<u64>().unwrap())
        })
        .collect()
}

fn individual_numbers(ranges: Vec<(u64, u64)>) -> Vec<u64> {
    ranges
        .into_iter()
        .flat_map(|(start, stop)| start..=stop)
        .collect()
}

fn int_doubled(id: u64) -> bool {
    let digits = id.to_string();

    doubled(digits)
}

fn doubled(digits: String) -> bool {
    let (first_half, second_half) = digits.split_at(digits.len() / 2);
    first_half == second_half
}

fn int_repeats(id: u64) -> bool {
    let digits = id.to_string().chars().collect();

    repeated(digits)
}

fn repeated(digits: Vec<char>) -> bool {
    let len = digits.len();
    if len < 2 {
        return false;
    }

    let max_pattern_len = len / 2;

    (1..=max_pattern_len).any(|len| {
        let pattern = digits.clone().into_iter().take(len).collect();
        is_repeated_pattern(digits.clone(), pattern)
    })
}

fn is_repeated_pattern(digits: Vec<char>, pattern: Vec<char>) -> bool {
    digits.chunks(pattern.len()).all(|p| p == pattern)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &str = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

    #[test]
    fn test_day02_main() {
        let (part1, part2) = main();
        assert_eq!(part1, 28146997880);
        assert_eq!(part2, 40028128307);
    }

    #[test]
    fn test_day02_part1() {
        let ranges = parse_ranges(EXAMPLE_INPUT);
        let all_ids = individual_numbers(ranges);
        let result = part1(&all_ids);
        assert_eq!(result, 1227775554);
    }

    #[test]
    fn test_day02_part2() {
        let ranges = parse_ranges(EXAMPLE_INPUT);
        let all_ids = individual_numbers(ranges);
        let result = part2(&all_ids);
        assert_eq!(result, 4174379265);
    }
}
