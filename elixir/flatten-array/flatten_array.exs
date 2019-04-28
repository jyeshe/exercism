defmodule FlattenArray do
  @doc """
    Accept a list and return the list flattened without nil values.

    ## Examples

      iex> FlattenArray.flatten([1, [2], 3, nil])
      [1,2,3]

      iex> FlattenArray.flatten([nil, nil])
      []

  """

  @spec flatten(list) :: list
  def flatten(list) do
    do_flatten(list, [])
  end

  defp do_flatten([], list), do: Enum.reverse(list)
  defp do_flatten([head | tail], list) when head == nil, do: do_flatten(tail, list)
  defp do_flatten([head | tail], list) when is_list(head) do
    flat_list = do_flatten(head, []) |> Enum.reverse()
    do_flatten(tail, flat_list ++ list)
  end
  defp do_flatten([head | tail], list), do: do_flatten(tail, [head | list])
end
