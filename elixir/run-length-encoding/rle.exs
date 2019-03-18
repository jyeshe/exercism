defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec encode(String.t()) :: String.t()
  def encode(string) do
    do_encode(String.graphemes(string), 0, nil, "")
  end

  defp do_encode([], 0, nil, ""), do: ""

  defp do_encode([], count, last, encoded) do
    concat_encoded(encoded, count, last)
  end

  defp do_encode([grapheme | tail], count, last, encoded) do
    if grapheme == last or last == nil do
      do_encode(tail, count+1, grapheme, encoded)
    else
      do_encode(tail, 1, grapheme, concat_encoded(encoded, count, last))
    end
  end

  defp concat_encoded(encoded, count, grapheme) when count == 1 do
    encoded <> grapheme
  end

  defp concat_encoded(encoded, count, grapheme) do
    encoded <> Integer.to_string(count) <> grapheme
  end

  @spec decode(String.t()) :: String.t()
  def decode(string) do
    do_decode(String.graphemes(string), "", "")
  end

  defp do_decode([], _, _), do: ""

  defp do_decode([grapheme], count, decoded) do
    concat_decoded(decoded, count, grapheme)
  end

  @digits_list for i <- 1..9, do: Integer.to_string(i)

  defp do_decode([grapheme | tail], count, decoded) do
    if grapheme in @digits_list do
      do_decode(tail, count <> grapheme, decoded)
    else
      do_decode(tail, "", concat_decoded(decoded, count, grapheme))
    end
  end

  defp concat_decoded(decoded, "", grapheme), do: decoded <> grapheme

  defp concat_decoded(decoded, count, grapheme) do
    {n, _} = Integer.parse(count)
    decoded <> String.duplicate(grapheme, n)
  end
end
