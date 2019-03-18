defmodule BracketPush do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str) do
    do_check_brackets(String.to_charlist(str), [])
  end

  defp do_check_brackets([], list), do: list == []

  defp do_check_brackets([c | tail], bracket_list) when c == ?{ or c == ?[ or c == ?( do
    do_check_brackets(tail, [c | bracket_list])
  end

  defp do_check_brackets([c | tail], [bracket_head | brackets_tail]) when c == ?} or c == ?] or c == ?) do
    is_paired(bracket_head, c) and do_check_brackets(tail, brackets_tail)
  end

  defp do_check_brackets([c | tail], bracket_list) do
    do_check_brackets(tail, bracket_list)
  end

  defp is_paired(open, close) do
    (open == ?{ and close == ?}) or (open == ?[ and close == ?]) or (open == ?( and close == ?))
  end
end
