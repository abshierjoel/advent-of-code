defmodule Day01 do
  @max_pos 100
  @starting_pos 50
  @matching_pos 0
  @path "src/day01/input.txt"

  def main do
    raw_file = File.read!(@path)
    turns = parse_directions(raw_file)
    part1(turns) |> IO.inspect(label: "Part 1: Times landed on 0")
    part2(turns) |> IO.inspect(label: "Part 2: Times passed through 0")
  end

  def part1(turns) do
    {_, zeros, _} = turn(turns)
    zeros
  end

  def part2(turns) do
    {_ending_pos, _zeros, match_count} = turn(turns)
    match_count
  end

  defp turn(turns) do
    Enum.reduce(turns, {@starting_pos, 0, 0}, fn {dir, steps},
                                                 {current_pos, zeros, match_count} ->
      case dir do
        :left ->
          {Integer.mod(current_pos - steps, @max_pos),
           (Integer.mod(@max_pos - current_pos, @max_pos) + steps) |> div(@max_pos)}

        :right ->
          {Integer.mod(current_pos + steps, @max_pos), (current_pos + steps) |> div(@max_pos)}
      end
      |> then(fn {new_pos, matches} ->
        {new_pos, zeros + if(new_pos == @matching_pos, do: 1, else: 0), match_count + matches}
      end)
    end)
  end

  def parse_directions(string) do
    String.split(string, "\n", trim: true)
    |> Enum.map(fn line ->
      {case String.first(line) do
         "L" -> :left
         "R" -> :right
         _ -> raise "Invalid direction"
       end, Integer.parse(String.slice(line, 1..-1//1)) |> elem(0)}
    end)
  end
end

Day01.main()
ExUnit.start()

defmodule Day01Test do
  use ExUnit.Case, async: true

  @example_input """
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82
  """

  describe "test parse_directions" do
    test "parses directions correctly" do
      result = Day01.parse_directions(@example_input)

      assert result == [
               {:left, 68},
               {:left, 30},
               {:right, 48},
               {:left, 5},
               {:right, 60},
               {:left, 55},
               {:left, 1},
               {:left, 99},
               {:right, 14},
               {:left, 82}
             ]
    end
  end

  test "part1 returns count of times landing on 0" do
    result = @example_input |> Day01.parse_directions() |> Day01.part1()
    assert result == 3
  end

  test "part2 returns count of times passing through 0" do
    result = @example_input |> Day01.parse_directions() |> Day01.part2()
    assert result == 6
  end
end
