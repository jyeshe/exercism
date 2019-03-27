defmodule StringSeries do
  @doc """
  Given a string `s` and a positive integer `size`, return all substrings
  of that size. If `size` is greater than the length of `s`, or less than 1,
  return an empty list.
  """
  @spec slices(s :: String.t(), size :: integer) :: list(String.t())
  def slices(s, size) when size <= 0, do: []
  def slices(s, size) do
    length = String.length(s)

    if length < size do
      []
    else
      last_index = length-size

      for index <- 0..last_index do
        String.slice(s, index, size)
      end
    end
  end
end
