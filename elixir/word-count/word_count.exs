defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map
  def count(sentence) do
    # get words without punctuation
    words = Regex.scan(~r/\w+\-\w+|\w+/u, sentence)
    # get strings in lowercase
    words_list = Enum.flat_map(words,
      fn [w] ->
        str_low = String.downcase(w);
        String.split(str_low, "_")
      end)

    # creates word => word_count map
    Enum.reduce(words_list, %{},
      fn word, count_map ->
        Map.update(count_map, word, 1, &(&1+1))
      end)
  end
end
