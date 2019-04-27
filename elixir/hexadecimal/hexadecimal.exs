defmodule Hexadecimal do
  @doc """
    Accept a string representing a hexadecimal value and returns the
    corresponding decimal value.
    It returns the integer 0 if the hexadecimal is invalid.
    Otherwise returns an integer representing the decimal value.

    ## Examples

      iex> Hexadecimal.to_decimal("invalid")
      0

      iex> Hexadecimal.to_decimal("af")
      175

  """

  @spec to_decimal(binary) :: integer
  def to_decimal(hex) do
    hex
      |> String.reverse()
      |> String.to_charlist()
      |> Enum.reduce_while({0, 0},
        fn chr, acc ->
          case sum(chr, acc) do
            {:error, _reason, bad_value_idx} ->
              {:halt, {bad_value_idx,0}}
            new_acc ->
              {:cont, new_acc}
          end
        end)
      |> elem(1)
      |> trunc()
  end

  def sum(chr, acc) when chr >= ?0 and chr <= ?9, do: do_sum(chr - ?0, acc)
  def sum(chr, acc) when chr >= ?a and chr <= ?f, do: do_sum(chr - ?a + 10, acc)
  def sum(chr, acc) when chr >= ?A and chr <= ?F, do: do_sum(chr - ?A + 10, acc)
  def sum(_, {acc_i, _}), do: {:error, "invalid char at", acc_i}

  def do_sum(number, {acc_i, acc_sum}), do: {acc_i+1, acc_sum + number * :math.pow(16, acc_i)}
end
