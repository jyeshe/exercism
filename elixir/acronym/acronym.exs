defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    string
      |> String.split([" ", "-"])
      |> Enum.reduce("",
          fn word, acc ->
            acronyms = find_upper_chars(word)
            first_char_upcase = String.first(word) |> String.upcase() |> String.to_charlist() |> List.first()
            acronyms = [first_char_upcase | acronyms] |> Enum.dedup()
            acc <> to_string(acronyms)
          end)
  end

  defp find_upper_chars(word), do: word |> String.to_charlist() |> Enum.filter(&(&1 >= ?A and &1 <= ?Z))
end
