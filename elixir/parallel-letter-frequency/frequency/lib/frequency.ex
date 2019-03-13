defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency([], _), do: %{}
  def frequency(texts, workers) do
    all_chars = Enum.flat_map(texts, fn text -> String.graphemes(text) end)
    length_per_worker = div(length(all_chars), workers)
    chunks = Enum.chunk_every(all_chars, length_per_worker+1)

    chunks
      |> Enum.map(fn chunk -> Task.async(fn -> count_letters(chunk) end) end)
      |> Enum.map(&(Task.await(&1, 60000)))
      |> merge_count_maps
  end

  def count_letters(graphemes) do
    Enum.reduce(graphemes, %{}, fn grapheme, acc ->
      if String.match?(grapheme, ~r/\p{L}/u) do
        downcased_letter = String.downcase(grapheme)
        Map.update(acc, downcased_letter, 1, &(&1+1))
      else
        acc
      end
    end)
  end

  defp merge_count_maps(maps) do
    do_merge(maps, %{})
  end

  defp do_merge([], result_map), do: result_map
  defp do_merge([map | other_maps], result_map) do
    do_merge(other_maps, Map.merge(map, result_map, fn _k, v1, v2 -> v1+v2 end))
  end
end
