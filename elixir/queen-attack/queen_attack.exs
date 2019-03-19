defmodule Queens do
  @type t :: %Queens{black: {integer, integer}, white: {integer, integer}}
  defstruct black: nil, white: nil

  @doc """
  Creates a new set of Queens
  """
  @spec new() :: Queens.t()
  @spec new({integer, integer}, {integer, integer}) :: Queens.t()
  def new(white \\ nil, black \\ nil) do
    # input or default
    white = white || {0, 3}
    black = black || {7, 3}

    cond do
      white == black ->
        raise ArgumentError
      true ->
        %Queens{black: black, white: white}
    end
  end

  @line_col_range 0..7

  @doc """
  Gives a string representation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(queens) do
    {bline, bcol} = queens.black
    {wline, wcol} = queens.white

    board_lines =
      for line <- @line_col_range do
        line_list =
          for col <- @line_col_range do
            cond do
              line == bline and col == bcol -> "B"
              line == wline and col == wcol -> "W"
              true -> "_"
            end
          end

        Enum.join(line_list, " ")
      end

    Enum.join(board_lines, "\n")
  end

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(queens) do
    {bline, bcol} = queens.black
    {wline, wcol} = queens.white


    bline == wline or bcol == wcol or same_diagonal(queens.black, queens.white)
  end

  defp same_diagonal({line1, col1}, {line2, col2}) do
    diagonals = Enum.flat_map(@line_col_range,
      fn line ->
        line_diff = abs(line - line1)

        [{line, col1-line_diff}, {line, col1+line_diff}]
      end)

    nil != Enum.find(diagonals, &(&1 == {line2, col2}))
  end
end
