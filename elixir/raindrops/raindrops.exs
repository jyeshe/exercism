defmodule Raindrops do
  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
    just pass the number's digits straight through.
  """
  @spec convert(pos_integer) :: String.t()
  def convert(number) do
    speak = raindrop(number, 3) <> raindrop(number, 5) <> raindrop(number, 7)

    cond do
        speak != "" -> speak
        true -> Integer.to_string(number)
    end
  end

  defp raindrop(num, 3) when rem(num, 3) == 0, do: "Pling"
  defp raindrop(_num, 3), do: ""

  defp raindrop(num, 5) when rem(num, 5) == 0, do: "Plang"
  defp raindrop(_num, 5), do: ""

  defp raindrop(num, 7) when rem(num, 7) == 0, do: "Plong"
  defp raindrop(_Num, 7), do: ""

end
