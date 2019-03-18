defmodule Luhn do
  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    sum = String.reverse(number) |> String.to_charlist() |> luhn_checksum(0, 0) # false <=> do not double
    rem(sum, 10) == 0
  end

  defp luhn_checksum([], index, _) when index == 0 or index == 1, do: -1

  defp luhn_checksum([], _, sum), do: sum

  # Ignores whitespaces
  defp luhn_checksum([digit | tail], index, sum) when digit == 32, do: luhn_checksum(tail, index, sum)

  # Only digits allowed
  defp luhn_checksum([digit | tail], index, sum) when digit >= ?0 and digit <= ?9 do
    new_sum = sum_digit(digit - ?0, index, sum)

    luhn_checksum(tail, index+1, new_sum)
  end

  # Invalidates sum for any other char
  defp luhn_checksum([_ | _], _, _), do: -1

  defp sum_digit(digit_int, index, sum) when rem(index, 2) == 0, do: sum + digit_int

  defp sum_digit(digit_int, _, sum) when digit_int > 4, do: sum + (2 * digit_int) - 9

  defp sum_digit(digit_int, _, sum), do: sum + 2 * digit_int

end
