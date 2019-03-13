defmodule Isogram do
  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t()) :: boolean
  def isogram?(sentence) do
    chars = String.graphemes(sentence)
    isogram(chars, %{})
  end

  defp isogram([], _), do: true
  defp isogram([chr | t], presence_map) do
    present? = Map.get(presence_map, chr, false)

    if present? and chr != " " and chr != "-" do
      false
    else
      isogram(t, Map.put(presence_map, chr, true))
    end
  end
end
