defmodule Day03 do
  @path "src/day03/input.txt"

  def main do
    raw_file = File.read!(@path)
    banks = parse_joltages(raw_file)
    part1(banks) |> IO.inspect(label: "Part 1: finding max joltages for 2 batteries")
    part2(banks) |> IO.inspect(label: "Part 2: finding max joltages for 12 batteries")
  end

  def part1(banks) do
    banks
    |> Enum.map(&find_max_n_joltages(&1, 2))
    |> Enum.map(fn {acc, _r} -> acc end)
    |> Enum.sum()
  end

  def part2(banks) do
    banks
    |> Enum.map(&find_max_n_joltages(&1, 12))
    |> Enum.map(fn {acc, _r} -> acc end)
    |> Enum.sum()
  end

  defp find_max_n_joltages(bank, batteries) do
    Enum.reduce((batteries - 1)..0//-1, {0, bank}, fn place, {acc, rem} ->
      {max_value, max_index} =
        rem
        |> Enum.with_index()
        |> Enum.take(length(rem) - place)
        |> Enum.max_by(fn {j, _i} -> j end)

      place = max_value * Integer.pow(10, place)
      new_rem = rem |> Enum.drop(max_index + 1)

      {acc + place, new_rem}
    end)
  end

  def parse_joltages(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map(fn bank -> String.split(bank, "", trim: true) end)
    |> Enum.map(fn bank ->
      Enum.map(bank, &String.to_integer/1)
    end)
  end
end

Day03.main()
ExUnit.start()

defmodule Day03Test do
  use ExUnit.Case, async: true

  @example_input """
  987654321111111
  811111111111119
  234234234234278
  818181911112111
  """

  test "part1" do
    result =
      @example_input |> Day03.parse_joltages() |> Day03.part1()

    assert result == 357
  end

  test "part2" do
    result =
      @example_input |> Day03.parse_joltages() |> Day03.part2()

    assert result == 3_121_910_778_619
  end
end
