defmodule Scrabble do

  @score_map %{
     [?A, ?E, ?I, ?O, ?U, ?L, ?N, ?R, ?S, ?T] => 1,
     [?D, ?G]             => 2,
     [?B, ?C, ?M, ?P]     => 3,
     [?F, ?H, ?V, ?W, ?Y] => 4,
     [?K]                 => 5,
     [?J, ?X]             => 8,
     [?Q, ?Z]             => 10,
  }

  @letter_score_map Enum.reduce(@score_map, %{},
    fn {letters,score}, map ->
      Enum.into(letters, map, fn letter -> {letter, score} end)
    end)

  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t()) :: non_neg_integer
  def score(word) do
    word
      |> String.trim()
      |> String.upcase()
      |> String.to_charlist()
      |> Enum.map(fn letter -> @letter_score_map[letter] end)
      |> Enum.sum()
  end
end
