defmodule PigLatin do
  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.

  Words beginning with consonants should have the consonant moved to the end of
  the word, followed by "ay".

  Words beginning with vowels (aeiou) should have "ay" added to the end of the
  word.

  Some groups of letters are treated like consonants, including "ch", "qu",
  "squ", "th", "thr", and "sch".

  Some groups are treated like vowels, including "yt" and "xr".
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
      |> String.split()
      |> Enum.map(&(translate_word/1))
      |> Enum.join(" ")
  end

  defp translate_word(word) do
    first_vowel_index =
      String.graphemes(word)
      |> Enum.find_index(fn grapheme -> grapheme in ["a", "e", "i", "o", "u"] end)

    first_letter = String.at(word, 0)

    first_vowel = String.at(word, first_vowel_index)
    before_first_vowel = if first_vowel_index == 0, do: "", else: String.at(word, first_vowel_index-1)

    cond do
      first_vowel_index == 0 or (first_letter in ["x","y"] and first_vowel_index > 1) ->
        word <> "ay"
      first_vowel == "u" and before_first_vowel == "q" ->
        String.slice(word, first_vowel_index+1, String.length(word)) <> String.slice(word, 0, first_vowel_index+1) <> "ay"
      true ->
        String.slice(word, first_vowel_index, String.length(word)) <> String.slice(word, 0, first_vowel_index) <> "ay"
    end
  end
end
