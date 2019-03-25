defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) when shift < 0 or shift > 26, do: :error
  def rotate(text, shift) do
    shifted_list =
      for <<chr>> <- String.codepoints(text) do
        cond do
          chr >= ?a and chr <= ?z ->
            shift_letter(chr, ?a, shift)
          chr >= ?A and chr <= ?Z ->
            shift_letter(chr, ?A, shift)
          true ->
            <<chr>>
        end
      end
    Enum.join(shifted_list)
  end

  defp shift_letter(chr, base, shift), do: <<base + rem(chr-base + shift, 26)>>
end
