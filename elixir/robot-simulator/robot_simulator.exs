defmodule RobotSimulator do
  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: atom, position :: {integer, integer}) :: any
  def create(direction \\ nil, position \\ nil)
  def create(direction,_) when direction not in [:north, :east, :south, :west, nil], do: {:error, "invalid direction"}
  def create(direction, {x, y} = position) when is_number(x) and is_number(y) do
    out_direction = if nil == direction, do: :north, else: direction
    {out_direction, position}
  end
  def create(nil,nil), do: {:north, {0,0}}
  def create(_, _), do: {:error, "invalid position"}

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: any, instructions :: String.t()) :: any
  def simulate(robot, instructions) do
    Enum.reduce_while(String.codepoints(instructions), robot,
      fn instruction, {direction, position} ->
        case turn(instruction, direction) do
          {:error, reason} ->
            {:halt, {:error, reason}}
          new_direction ->
            acc = {new_direction, advance(instruction, new_direction, position)}
            {:cont, acc}
        end
      end)
  end

  defp turn("R", direction) do
    case direction do
      :north -> :east
      :east -> :south
      :south -> :west
      :west -> :north
    end
  end

  defp turn("L", direction) do
    case direction do
      :north -> :west
      :west -> :south
      :south -> :east
      :east -> :north
    end
  end
  defp turn("A", direction), do: direction
  defp turn(_, _), do: {:error, "invalid instruction"}

  defp advance("A", :north, {x,y}), do: {x, y+1}
  defp advance("A", :west,  {x,y}), do: {x-1, y}
  defp advance("A", :south, {x,y}), do: {x, y-1}
  defp advance("A", :east, {x,y}), do: {x+1, y}
  defp advance(_, _, position), do: position

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: any) :: atom
  def direction(robot) do
    elem(robot, 0)
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: any) :: {integer, integer}
  def position(robot) do
    elem(robot, 1)
  end
end
