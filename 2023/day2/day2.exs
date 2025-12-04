defmodule Day2PartA do
  @max_red 12
  @max_green 13
  @max_blue 14

  def run(filename) do
    filename
    |> File.stream!()
    |> Stream.with_index()
    |> Stream.map(&read_game/1)
    |> Stream.filter(fn {game, _round} ->
      Enum.all?(game, fn draw ->
        Map.get(draw, :red, 0) <= @max_red &&
          Map.get(draw, :green, 0) <= @max_green &&
          Map.get(draw, :blue, 0) <= @max_blue
      end)
    end)
    |> Stream.map(fn {_game, i} -> i end)
    |> Enum.sum()
    |> IO.puts()
  end

  def read_game({game, round}) do
    game
    |> String.split([":", ";"])
    |> tl()
    |> Enum.map(&read_draw/1)
    |> then(&{&1, round + 1})
  end

  def read_draw(draw) do
    draw
    |> String.split(",")
    |> Enum.reduce(%{}, fn set, acc ->
      [n, c] =
        set
        |> String.trim()
        |> String.split(" ")

      Map.put(acc, String.to_atom(c), String.to_integer(n))
    end)
  end
end

defmodule Day2PartB do
  @max_red 12
  @max_green 13
  @max_blue 14

  def run(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&read_game/1)
    |> Stream.map(fn game ->
      Enum.reduce(game, %{}, fn draw, acc ->
        Map.merge(acc, draw, fn _k, v1, v2 -> max(v1, v2) end)
      end)
    end)
    |> Stream.map(fn g -> Map.get(g, :red, 1) * Map.get(g, :green, 1) * Map.get(g, :blue, 1) end)
    |> Enum.sum()
    |> IO.inspect()
  end

  def read_game(game) do
    game
    |> String.split([":", ";"])
    |> tl()
    |> Enum.map(&read_draw/1)
  end

  def read_draw(draw) do
    draw
    |> String.split(",")
    |> Enum.reduce(%{}, fn set, acc ->
      [n, c] =
        set
        |> String.trim()
        |> String.split(" ")

      Map.put(acc, String.to_atom(c), String.to_integer(n))
    end)
  end
end

Day2PartA.run("example.txt")
Day2PartA.run("input.txt")

Day2PartB.run("example.txt")
Day2PartB.run("input.txt")
