defmodule Bob do

  def is_question(""), do: false
  def is_question(string), do: String.last(string) == "?"

  def is_yelling(""), do: false
  def is_yelling(string) do
    words = Regex.scan(~r/\p{L}+/u, string)
    string == String.upcase(string) and length(words) > 0
  end

  def hey(input) do
    trimed_input = String.trim(input)

    cond do
      is_question(trimed_input) and is_yelling(trimed_input) ->
        "Calm down, I know what I'm doing!"
      is_question(trimed_input) ->
        "Sure."
      is_yelling(trimed_input) ->
        "Whoa, chill out!";
      trimed_input == "" ->
        "Fine. Be that way!";
      true ->
        "Whatever."
    end
  end
end
