defmodule SaddlePoints do
  @doc """
  Parses a string representation of a matrix
  to a list of rows
  """
  @spec rows(String.t()) :: [[integer]]
  def rows(str) do
    rows_str = String.split(str, "\n")

    for row_str <- rows_str do
      num_str_list = String.split(row_str, " ")

      for num_str <- num_str_list do
        {num, _} = Integer.parse(num_str)
        num
      end
    end
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(str) do
    col_map = get_column_map(str)

    Map.values(col_map)
  end

  defp get_column_map(str_input) do
    # rows in separate strings
    rows_str = String.split(str_input, "\n")

    Enum.reduce(rows_str, %{},
    fn (row_str, col_map) ->
      # list of numbers (in string format) of a row
      num_str_list = String.split(row_str, " ")

      {new_col_map, _} =
        Enum.reduce(num_str_list, {col_map, 0},
          fn num_str, {map, index} ->
            column = Map.get(map, index, [])
            {num, _} = Integer.parse(num_str)

            {Map.put(map, index, column ++ [num]), index+1}
          end)
      new_col_map
    end)
  end

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(str) do
    rows = rows(str)
    col_map = get_column_map(str)

    {ret_list, _} =
      Enum.reduce(rows, {[], 0},
      fn row, acc ->
        {saddle_list, row_index} = acc
        {saddle_candidate, col_index} = get_row_max(row)

        column = col_map[col_index]
        min = Enum.min(column)

        cond do
          min == saddle_candidate ->
            new_saddle_list = saddle_list ++ [{row_index, col_index}]
            {new_saddle_list, row_index+1}
          true ->
            {saddle_list, row_index+1}
        end
      end)

    ret_list
  end

  # returns the max number of a row and its index
  defp get_row_max(row) do
    {max, max_col_index, _} = Enum.reduce(row, {0, 0, 0},
      fn num, {max, max_index, j} ->
        cond do
          num > max -> {num, j, j+1}
          true ->      {max, max_index, j+1}
        end
      end)
    {max, max_col_index}
  end
end
