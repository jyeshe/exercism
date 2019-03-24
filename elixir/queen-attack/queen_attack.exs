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

  @row_col_range 0..7

  @doc """
  Gives a string representation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(queens) do
    {b_row, b_col} = queens.black
    {w_row, w_col} = queens.white

    board_rows_str =
      for row <- @row_col_range do
        row_list =
          for col <- @row_col_range do
            cond do
              row == b_row and col == b_col -> "B"
              row == w_row and col == w_col -> "W"
              true -> "_"
            end
          end

        Enum.join(row_list, " ")
      end

    Enum.join(board_rows_str, "\n")
  end

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(queens) do
    {b_row, b_col} = queens.black
    {w_row, w_col} = queens.white

    b_row == w_row or b_col == w_col or same_diagonal(queens.black, queens.white)
  end

  defp same_diagonal({row1, col1}, {row2, col2}) do
    diagonals = Enum.flat_map(@row_col_range,
      fn row ->
        row_diff = abs(row - row1)

        [{row, col1-row_diff}, {row, col1+row_diff}]
      end)

    nil != Enum.find(diagonals, &(&1 == {row2, col2}))
  end
end
