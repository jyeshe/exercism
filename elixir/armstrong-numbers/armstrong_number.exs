defmodule ArmstrongNumber do
  @moduledoc """
  Provides a way to validate whether or not a number is an Armstrong number
  """

  @spec valid?(integer) :: boolean
  def valid?(number) when is_integer(number) do
    digits = Integer.digits(number)
    num_digits = length(digits)

    sum = Enum.reduce(digits, 0,
      fn digit, acc ->
        acc + :math.pow(digit, num_digits)
      end)

    number == sum
  end
end
