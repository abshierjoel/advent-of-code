defmodule Day02 do
  @path "src/day02/input.txt"

  def main do
    raw_file = File.read!(@path)
    ranges = parse_ranges(raw_file)
    all_ids = individual_numbers(ranges)
    part1(all_ids) |> IO.inspect(label: "Part 1: strings that are mirrored")
    part2(all_ids) |> IO.inspect(label: "Part 2: strings that ahve repeating patterns")
  end

  def part1(ranges) do
    ranges
    |> Enum.filter(&int_doubled?/1)
    |> Enum.sum()
  end

  def part2(ranges) do
    ranges
    |> Enum.filter(&int_repeats?/1)
    |> Enum.sum()
  end

  def parse_ranges(string) do
    string
    |> String.split(",", trim: true)
    |> Enum.map(fn range ->
      [start_str, end_str] = String.split(range, "-", trim: true)
      {String.to_integer(start_str), String.to_integer(end_str)}
    end)
  end

  def individual_numbers(ranges) do
    Enum.flat_map(ranges, fn {start_range, end_range} ->
      start_range..end_range
    end)
  end

  defp int_doubled?(id) when is_integer(id), do: id |> Integer.digits() |> doubled?()

  defp doubled?(digits) do
    {first_half, second_half} = Enum.split(digits, div(length(digits), 2))
    first_half == second_half
  end

  defp int_repeats?(id) when is_integer(id), do: id |> Integer.digits() |> repeats?()

  defp repeats?(digits) when length(digits) >= 2 do
    len = length(digits)
    max_pattern_len = div(len, 2)

    Enum.any?(1..max_pattern_len, fn pattern_len ->
      pattern = Enum.take(digits, pattern_len)
      is_pattern_repeated?(digits, pattern)
    end)
  end

  defp repeats?(_), do: false

  defp is_pattern_repeated?(digits, pattern) do
    digits
    |> Enum.chunk_every(length(pattern))
    |> Enum.all?(&(pattern == &1))
  end

  ### Originally I exhaustively matched all cases for the max length
  # defp doubled?([a, a]), do: true
  # defp doubled?([a, b, a, b]), do: true
  # defp doubled?([a, b, c, a, b, c]), do: true
  # defp doubled?([a, b, c, d, a, b, c, d]), do: true
  # defp doubled?([a, b, c, d, e, a, b, c, d, e]), do: true
  # defp doubled?([a, b, c, d, e, f, a, b, c, d, e, f]), do: true
  # defp doubled?(_), do: false
  # defp repeats?([a, a]), do: true
  # defp repeats?([a, a, a]), do: true
  # defp repeats?([a, a, a, a]), do: true
  # defp repeats?([a, a, a, a, a]), do: true
  # defp repeats?([a, a, a, a, a, a]), do: true
  # defp repeats?([a, a, a, a, a, a, a]), do: true
  # defp repeats?([a, a, a, a, a, a, a, a]), do: true
  # defp repeats?([a, a, a, a, a, a, a, a, a]), do: true
  # defp repeats?([a, a, a, a, a, a, a, a, a, a]), do: true
  # defp repeats?([a, b, a, b]), do: true
  # defp repeats?([a, b, a, b, a, b]), do: true
  # defp repeats?([a, b, a, b, a, b, a, b]), do: true
  # defp repeats?([a, b, a, b, a, b, a, b, a, b]), do: true
  # defp repeats?([a, b, a, b, a, b, a, b, a, b, a, b]), do: true
  # defp repeats?([a, a, b, b, a, a, b, b]), do: true
  # defp repeats?([a, a, b, b, a, a, b, b, a, a, b, b]), do: true
  # defp repeats?([a, a, a, a, b, b, b, b]), do: true
  # defp repeats?([a, a, a, a, b, b, b, b, a, a, a, b, b, b]), do: true
  # defp repeats?([a, b, c, a, b, c]), do: true
  # defp repeats?([a, b, c, a, b, c, a, b, c]), do: true
  # defp repeats?([a, b, c, a, b, c, a, b, c, a, b, c]), do: true
  # defp repeats?([a, a, b, b, c, c, a, a, b, b, c, c]), do: true
  # defp repeats?([a, a, b, b, c, c, a, a, b, b, c, c, a, a, b, b, c, c]), do: true
  # defp repeats?([a, b, c, d, a, b, c, d]), do: true
  # defp repeats?([a, b, c, d, a, b, c, d, a, b, c, d]), do: true
  # defp repeats?([a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d]), do: true
  # defp repeats?([a, a, b, b, c, c, d, d, a, a, b, b, c, c, d, d]), do: true
  # defp repeats?([a, b, c, d, e, a, b, c, d, e]), do: true
  # defp repeats?([a, b, c, d, e, a, b, c, d, e, a, b, c, d, e]), do: true
  # defp repeats?([a, b, c, d, e, f, a, b, c, d, e, f]), do: true
  # defp repeats?([a, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f]), do: true
  # defp repeats?(_), do: false
end

Day02.main()
ExUnit.start()

defmodule Day02Test do
  use ExUnit.Case, async: true

  @example_input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

  test "part1" do
    result = @example_input |> Day02.parse_ranges() |> Day02.individual_numbers() |> Day02.part1()
    assert result == 1_227_775_554
  end

  test "part2" do
    result = @example_input |> Day02.parse_ranges() |> Day02.individual_numbers() |> Day02.part2()
    assert result == 4_174_379_265
  end
end
