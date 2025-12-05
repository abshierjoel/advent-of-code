use std::fs;

const INPUT_PATH: &str = "src/day03/input.txt";

fn main() -> (u64, u64) {
    let raw_file = fs::read_to_string(INPUT_PATH).unwrap();
    let banks = parse_joltages(&raw_file);

    let part1 = part1(banks.clone());
    println!("Part 1: finding max joltages for 2 batteries: {}", part1);
    let part2 = part2(banks.clone());
    println!("Part 2: finding max joltages for 12 batteries: {}", part2);

    (part1, part2)
}

fn part1(banks: Vec<Vec<u32>>) -> u64 {
    banks
        .into_iter()
        .map(|bank| find_max_n_joltages(bank, 2).0)
        .sum()
}

fn part2(banks: Vec<Vec<u32>>) -> u64 {
    banks
        .into_iter()
        .map(|bank| find_max_n_joltages(bank, 12).0)
        .sum()
}

fn parse_joltages(raw_file: &str) -> Vec<Vec<u32>> {
    raw_file
        .lines()
        .map(|bank| bank.chars().map(|b| b.to_digit(10).unwrap()).collect())
        .collect()
}

fn find_max_n_joltages(bank: Vec<u32>, batteries: usize) -> (u64, Vec<u32>) {
    (0..batteries)
        .rev()
        .fold((0u64, bank.to_vec()), |(acc, rem), place| {
            let searchable_len = rem.len() - place;
            let (max_index, &max_value) = rem[..searchable_len]
                .iter()
                .enumerate()
                .reduce(
                    |(max_i, max_v), (i, v)| {
                        if v > max_v { (i, v) } else { (max_i, max_v) }
                    },
                )
                .unwrap();

            let new_place = (max_value as u64) * 10u64.pow(place as u32);
            let new_rem = rem[(max_index + 1)..].to_vec();

            (acc + new_place, new_rem)
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &str = "987654321111111
811111111111119
234234234234278
818181911112111";

    #[test]
    fn test_day03_main() {
        let (part1, part2) = main();
        assert_eq!(part1, 17324);
        assert_eq!(part2, 171846613143331);
    }

    #[test]
    fn test_day03_part1() {
        let banks = parse_joltages(EXAMPLE_INPUT);
        let result = part1(banks);
        assert_eq!(result, 357);
    }

    #[test]
    fn test_day03_part2() {
        let banks = parse_joltages(EXAMPLE_INPUT);
        let result = part2(banks);
        assert_eq!(result, 3_121_910_778_619);
    }
}
