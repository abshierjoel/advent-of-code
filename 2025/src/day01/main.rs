use std::error::Error;
use std::fs;

const MIN_POS: u32 = 0;
const MAX_POS: u32 = 99;
const STARTING_POS: u32 = 50;
const MATCHING_POS: u32 = 0;

fn main() -> Result<(), Box<dyn Error>> {
    let path = "src/day01/input.txt";
    let combination = CombinationLock(STARTING_POS);
    let input = read_file(path);
    let directions = lines_to_directions(&input);
    let result_part1 = dial_combianation_part_1(directions.clone(), combination);
    println!("Part 1 Result: {}", result_part1);

    let combination = CombinationLock(STARTING_POS);
    let result_part2 = dial_combianation_part_2(directions, combination);
    println!("Part 2 Result: {}", result_part2);

    Ok(())
}

fn read_file(path: &str) -> String {
    fs::read_to_string(path).expect("Failed to read file")
}

// Converts a string delimited by newlines to a vector of Turn structs
fn lines_to_directions(input: &str) -> Vec<Turn> {
    input.lines().map(From::from).collect()
}

// Takes a list of Turns and computes how many times the dial returns to 0
fn dial_combianation_part_1(turns: Vec<Turn>, mut combination: CombinationLock) -> u32 {
    let mut match_count = 0;

    for t in turns.iter() {
        combination = combination.turn(t);
        if combination.is_in_matching_position() {
            match_count += 1;
        }
    }

    match_count
}

// Takes a list of Turns and computes how many times the dial passes 0
fn dial_combianation_part_2(turns: Vec<Turn>, mut combination: CombinationLock) -> u32 {
    let mut match_count = 0;

    for t in turns.iter() {
        let res = combination.turn_counting_zeros(t);
        combination = res.0;
        match_count += res.1;
    }

    match_count
}

#[derive(PartialEq, Debug, Clone)]
struct Turn {
    direction: Direction,
    steps: u32,
}

impl From<&str> for Turn {
    fn from(value: &str) -> Turn {
        let steps: u32 = value[1..].parse().unwrap();
        match &value[0..1] {
            "L" => Turn::new(Direction::Left, steps),
            "R" => Turn::new(Direction::Right, steps),
            _ => panic!("Invalid direction"),
        }
    }
}

impl Turn {
    pub fn new(direction: Direction, steps: u32) -> Self {
        Turn { direction, steps }
    }
}

#[derive(PartialEq, Debug, Clone)]
enum Direction {
    Left,
    Right,
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Direction::Left => write!(f, "LEFT"),
            Direction::Right => write!(f, "RIGHT"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct CombinationLock(u32);

impl CombinationLock {
    pub fn new(pos: u32) -> Self {
        if is_position_valid(pos) {
            CombinationLock(pos)
        } else {
            panic!("Invalid starting position")
        }
    }

    pub fn turn(&self, turn: &Turn) -> Self {
        match turn.direction {
            Direction::Left => {
                let reduced_turns = turn.steps % (MAX_POS + 1);
                match self.0.checked_sub(reduced_turns) {
                    Some(s) => CombinationLock::new(s),
                    None => CombinationLock::new((MAX_POS + 1) - (reduced_turns - self.0)),
                }
            }
            Direction::Right => {
                if is_position_valid(self.0 + turn.steps) {
                    CombinationLock::new(self.0 + turn.steps)
                } else {
                    CombinationLock::new((self.0 + turn.steps) % (MAX_POS + 1))
                }
            }
        }
    }

    pub fn turn_counting_zeros(&self, turn: &Turn) -> (Self, u32) {
        let mut matches = 0;
        matches += turn.steps / (MAX_POS + 1);

        let new_lock = match turn.direction {
            Direction::Left => {
                let reduced_turns = turn.steps % (MAX_POS + 1);
                if let Some(s) = self.0.checked_sub(reduced_turns) {
                    if s == MATCHING_POS && self.0 != MATCHING_POS {
                        matches += 1;
                    }
                    CombinationLock::new(s)
                } else {
                    let new_pos = (MAX_POS + 1) - (reduced_turns - self.0);
                    if self.0 != MATCHING_POS && new_pos != MATCHING_POS {
                        matches += 1;
                    }
                    if new_pos == MATCHING_POS {
                        matches += 1;
                    }
                    CombinationLock::new(new_pos)
                }
            }
            Direction::Right => {
                let reduced_turns = turn.steps % (MAX_POS + 1);
                if self.0 + reduced_turns <= MAX_POS {
                    let new_pos = self.0 + reduced_turns;
                    if new_pos == MATCHING_POS && self.0 != MATCHING_POS {
                        matches += 1;
                    }
                    CombinationLock::new(new_pos)
                } else {
                    let new_pos = (self.0 + reduced_turns) % (MAX_POS + 1);
                    if self.0 != MATCHING_POS && new_pos != MATCHING_POS {
                        matches += 1;
                    }
                    if new_pos == MATCHING_POS {
                        matches += 1;
                    }
                    CombinationLock::new(new_pos)
                }
            }
        };

        (new_lock, matches)
    }

    pub fn is_in_matching_position(&self) -> bool {
        self.0 == MATCHING_POS
    }
}

fn is_position_valid(pos: u32) -> bool {
    pos >= MIN_POS && pos <= MAX_POS
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &str = r#"L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"#;

    #[test]
    fn test_main() {
        let _ = main();
    }

    #[test]
    fn test_lines_to_directions() {
        let directions = lines_to_directions(EXAMPLE_INPUT);

        assert_eq!(directions.len(), 10);
        assert_eq!(directions[0].direction, Direction::Left);
        assert_eq!(directions[0].steps, 68);
        assert_eq!(directions[1].direction, Direction::Left);
        assert_eq!(directions[1].steps, 30);
        assert_eq!(directions[2].direction, Direction::Right);
        assert_eq!(directions[2].steps, 48);
        assert_eq!(directions[3].direction, Direction::Left);
        assert_eq!(directions[3].steps, 5);
        assert_eq!(directions[4].direction, Direction::Right);
        assert_eq!(directions[4].steps, 60);
        assert_eq!(directions[5].direction, Direction::Left);
        assert_eq!(directions[5].steps, 55);
        assert_eq!(directions[6].direction, Direction::Left);
        assert_eq!(directions[6].steps, 1);
        assert_eq!(directions[7].direction, Direction::Left);
        assert_eq!(directions[7].steps, 99);
        assert_eq!(directions[8].direction, Direction::Right);
        assert_eq!(directions[8].steps, 14);
        assert_eq!(directions[9].direction, Direction::Left);
        assert_eq!(directions[9].steps, 82);
    }

    #[test]
    fn test_dial_combination_part_1_returns_count_of_times_landing_on_0() {
        let directions = lines_to_directions(EXAMPLE_INPUT);
        let combination = CombinationLock::new(STARTING_POS);
        let result = dial_combianation_part_1(directions, combination);
        let expected = 3;

        assert_eq!(result, expected);
    }

    #[test]
    fn test_dial_combination_part_2_returns_count_of_times_landing_on_0() {
        let directions = lines_to_directions(EXAMPLE_INPUT);
        let combination = CombinationLock::new(STARTING_POS);
        let result = dial_combianation_part_2(directions, combination);
        let expected = 6;

        assert_eq!(result, expected);
    }
}
