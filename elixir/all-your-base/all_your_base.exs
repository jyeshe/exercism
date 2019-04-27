defmodule AllYourBase do
  @doc """
  Given a number in base a, represented as a sequence of digits, converts it to base b,
  or returns nil if either of the bases are less than 2
  """

  @spec convert(list, integer, integer) :: list
  def convert(_, base_a, base_b) when base_a < 2 or base_b < 2, do: nil
  def convert([], _, _), do: nil
  def convert([0], _, _), do: [0]
  def convert([0 | tail], base_a, base_b), do: convert(tail, base_a, base_b)

  def convert(digits, base_a, base_b) do
    digits
      |> list_to_number(base_a)
      |> number_to_list(base_b)
  end

  def list_to_number(digits, base) do
    digits
      |> Enum.reverse()
      |> Enum.reduce_while({1, 0},
        fn digit, acc ->
          case sum_digit(digit, base, acc) do
            nil -> {:halt, {0, nil}}
            new_acc -> {:cont, new_acc}
          end
        end)
      |> elem(1)
  end

  defp sum_digit(digit, base, _) when digit < 0 or digit >= base, do: nil
  defp sum_digit(digit, base, {pow, acc_sum}), do: {base * pow, acc_sum + digit * pow}

  # 67 -> [1, 0, 3] base 8,
    # 67 -> div(67,64) -> 1
      # 67 - 64 = 3
    # 3 -> div(3, 8) -> 0
      # 3 - 0 = 3
    # 3 -> div(3, 1) -> 3
      # 3 - 3 = 3

  def number_to_list(nil, _), do: nil
  def number_to_list(number, base) do
    initial_pow = max_pow(number, base, base)
    divide_pow(number, initial_pow, base, [])
  end

  defp max_pow(number, base, pow) when number < pow, do: div(pow, base)
  defp max_pow(number, base, pow), do: max_pow(number, base, base*pow)

  # base case
  defp divide_pow(last_digit, 1, _, digits), do: digits ++ [last_digit]

  defp divide_pow(number, pow, base, list) do
    inspect "#{number} #{pow} #{base}"
    digit = div(number, pow)
    digit_pow = digit * pow
    next_pow = div(pow,base)

    divide_pow(number-digit_pow, next_pow, base, list ++ [digit])
  end
end
