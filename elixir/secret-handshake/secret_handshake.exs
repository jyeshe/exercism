defmodule SecretHandshake do

  use Bitwise

  @codelist [{0b1, "wink"}, {0b10, "double blink"}, {0b100, "close your eyes"}, {0b1000, "jump"}]

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) when (code &&& 0b10000) == 0b10000 do
    do_commands(code, :lists.reverse(@codelist))
  end
  def commands(code) do
    do_commands(code, @codelist)
  end

  defp do_commands(code, list) do
    for {secret, action} <- list, (secret &&& code) == secret, do: action
  end

end
