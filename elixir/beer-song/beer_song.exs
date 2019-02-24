defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(number) do
    case number do
      0 ->
        """
        No more bottles of beer on the wall, no more bottles of beer.
        Go to the store and buy some more, 99 bottles of beer on the wall.
        """
      1 ->
        """
        1 bottle of beer on the wall, 1 bottle of beer.
        Take it down and pass it around, no more bottles of beer on the wall.
        """
      n ->
        bottle_left_word = if n-1 == 1, do: "bottle", else: "bottles"
        """
        #{n} bottles of beer on the wall, #{n} bottles of beer.
        Take one down and pass it around, #{n-1} #{bottle_left_word} of beer on the wall.
        """
    end
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range) do
    lyrics_verses = for i <- range, do: verse(i)
    Enum.join(lyrics_verses, "\n")
  end

  @doc """
  Get the entire beer song.
  """
  def lyrics() do
    lyrics(99..0)
  end
end
