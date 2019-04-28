defmodule Spiral do

  @doc """
  Given the dimension, return a square matrix of numbers in clockwise spiral order.
  """
  @spec matrix(dimension :: integer) :: list(list(integer))
  def matrix(0), do: []
  def matrix(1), do: [[1]]
  def matrix(dimension) do

    # 1 2 %{right: 2, down: 1, left: 1, up: 0}
    # 4 3

    # 1 2 3 %{right: 3, down: 2, left: 2, up: 1}
    # 8 9 4 %{right: 1, down: 0}
    # 7 6 5

    # 1  2  3 4     %{right: 4, down: 3, left: 3, up: 2}
    # 12 13 14 5    %{right: 2, down: 1, left: 1, up: 0}
    # 11 16 15 6
    # 10  9  8 7

    # 1  2  3  4  5    %{right: 5, down: 4, left: 4, up: 3}
    # 16 17 18 19 6    %{right: 3, down: 2, left: 2, up: 1}
    # 15 24 25 20 7    %{right: 1, down: 0}
    # 14 23 22 21 8
    # 13 12 11 10 9

    # walking steps
    steps = %{right: dimension, down: dimension-1, left: dimension-1, up: dimension-2}

    # matrix = %{1 => %{}, 2 => %{}, ...} maps row numbers to rows
    matrix = for i <- 1..dimension, into: %{}, do: {i, %{}}
    # row 1 = %{1 => 1, 2, => 2, ..., dimension => dimension}
    row1 = for i <- 1..dimension, into: %{}, do: {i, i}
    # walks first row
    matrix = %{matrix | 1 => row1}
    # total walking length
    total_steps = dimension*dimension
    # initial {row, col} after walking first row
    initial_row_col = {2, dimension}
    # next_step_value
    next_step_value = dimension+1
    # start walking down (after first row)
    final_matrix = walk(:down, steps, initial_row_col, next_step_value, matrix, total_steps)

    Enum.map(final_matrix, fn {_row_num, matrix_row} -> Map.values(matrix_row) end)
  end

  def walk(_, _, _, step_value, matrix, total_steps) when step_value > total_steps, do: matrix
  def walk(:down, steps, {row, col}, step_value, matrix, total_steps) do
    # walk length
    num_steps = steps.down
    # update column
    new_matrix = Enum.reduce(0..num_steps-1, matrix,
      fn i,matrix_acc ->
        cur_row = row+i
        matrix_row = matrix_acc[cur_row]
        %{matrix_acc | cur_row => Map.put(matrix_row, col, step_value+i)}
      end)

    new_step_value = step_value + num_steps

    walk(:left, steps, {row+(num_steps-1), col-1}, new_step_value, new_matrix, total_steps)
  end

  def walk(:left, steps, {row, col}, step_value, matrix, total_steps) do
    # walk length
    num_steps = steps.left
    # update row
    new_matrix = Enum.reduce(0..num_steps-1, matrix,
      fn i,matrix_acc ->
        matrix_row = matrix_acc[row]
        cur_col = col-i
        %{matrix_acc | row => Map.put(matrix_row, cur_col, step_value+i)}
      end)

    new_step_value = step_value + num_steps

    walk(:up, steps, {row-1, col-(num_steps-1)}, new_step_value, new_matrix, total_steps)
  end

  def walk(:up, steps, {row, col}, step_value, matrix, total_steps) do
    # walk length
    num_steps = steps.up
    # update col
    new_matrix = Enum.reduce(0..num_steps-1, matrix,
      fn i,matrix_acc ->
        cur_row = row-i
        matrix_row = matrix_acc[cur_row]
        %{matrix_acc | cur_row => Map.put(matrix_row, col, step_value+i)}
      end)

    new_step_value = step_value + num_steps

    # %{right: 4, down: 3, left: 3, up: 2}
    # %{right: 2, down: 1, left: 1, up: 0}
    # walking steps
    steps = %{right: steps.right-2, down: steps.down-2, left: steps.left-2, up: steps.up-2}

    walk(:right, steps, {row-(num_steps-1), col+1}, new_step_value, new_matrix, total_steps)
  end

  def walk(:right, steps, {row, col}, step_value, matrix, total_steps) do
    # walk length
    num_steps = steps.right
    # update row
    new_matrix = Enum.reduce(0..num_steps-1, matrix,
      fn i,matrix_acc ->
        matrix_row = matrix_acc[row]
        cur_col = col+i
        %{matrix_acc | row => Map.put(matrix_row, cur_col, step_value+i)}
      end)

    new_step_value = step_value + num_steps

    walk(:down, steps, {row+1, col+(num_steps-1)}, new_step_value, new_matrix, total_steps)
  end

end
