defmodule TwelveDays do

  @gifts %{
    12 => "twelve Drummers Drumming",
    11 => "eleven Pipers Piping",
    10 => "ten Lords-a-Leaping",
    9  => "nine Ladies Dancing",
    8  => "eight Maids-a-Milking",
    7  => "seven Swans-a-Swimming",
    6  => "six Geese-a-Laying",
    5  => "five Gold Rings",
    4  => "four Calling Birds",
    3  => "three French Hens",
    2  => "two Turtle Doves",
    1  => "a Partridge in a Pear Tree."
  }

  @day %{
    1 => "first",
    2 => "second",
    3 => "third",
    4 => "fourth",
    5 => "fifth",
    6 => "sixth",
    7 => "seventh",
    8 => "eighth",
    9 => "ninth",
    10 => "tenth",
    11 => "eleventh",
    12 => "twelfth",
  }

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """
  @spec verse(number :: integer) :: String.t()
  def verse(1), do: day_phrase(1) <> @gifts[1]

  def verse(number) do
    gifts = for i <- number..2, do: @gifts[i]
    day_phrase(number) <> Enum.join(gifts, ", ") <> ", and " <> @gifts[1]
  end

  defp day_phrase(number) do
    "On the #{@day[number]} day of Christmas my true love gave to me: "
  end

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
    lines = for number <- starting_verse..ending_verse, do: verse(number)
    Enum.join(lines, "\n")
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing() :: String.t()
  def sing do
    verses(1, 12)
  end
end
