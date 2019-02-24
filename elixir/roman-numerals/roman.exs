defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) do
    roman_thousands(number)
  end

  defp roman_sequence(repeat_n, letter), do: Enum.join(for i <- 1..repeat_n, do: letter)

  defp roman_thousands(number) do
    thousands = div(number, 1000)

    if thousands ==0 do
      roman_hundreds(number, "")
    else
      first_roman_string = roman_sequence(thousands, "M")
      roman_hundreds(number - thousands*1000, first_roman_string)
    end
  end

  defp roman_hundreds(number, roman_string) do
    hundreds = div(number, 100)
    subtracted_number = number - hundreds*100
    new_roman_string = add_roman(hundreds, roman_string, :hundreds)
    roman_tens(subtracted_number, new_roman_string)
  end

  defp roman_tens(number, roman_string) do
    tens = div(number, 10)
    subtracted_number = number - tens*10
    new_roman_string = add_roman(tens, roman_string, :tens)
    roman_units(subtracted_number, new_roman_string)
  end

  defp roman_units(number, roman_string) do
    units = number
    add_roman(units, roman_string, :units)
  end

  defp add_roman(amount, roman_string, unit_type) do

    {unit, fourth, middle, sixth, nineth} = get_roman_letters(unit_type)

    cond do
      amount == 0 ->
        roman_string
      amount < 4 ->
        new_roman_string = roman_string <> roman_sequence(amount, unit)
      amount == 4 ->
        new_roman_string = roman_string <> fourth
      amount == 5 ->
        new_roman_string = roman_string <> middle
      amount == 6 ->
        new_roman_string = roman_string <> sixth
      amount < 9 ->
        new_roman_string = roman_string <> middle <> roman_sequence(amount-5, unit)
      amount == 9 ->
        new_roman_string = roman_string <> nineth
    end
  end

  defp get_roman_letters(unit_type) do
    case unit_type do
      :hundreds ->
        {"C", "CD", "D", "DC", "CM"}
      :tens ->
        {"X", "XL", "L", "LX", "XC"}
      :units ->
        {"I", "IV", "V", "VI", "IX"}
    end
  end
end
