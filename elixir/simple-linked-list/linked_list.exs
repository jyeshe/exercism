defmodule LinkedList do
  @opaque t :: tuple()

  @doc """
  Construct a new LinkedList
  """
  @spec new() :: t
  def new() do
    {[]}
  end

  @doc """
  Push an item onto a LinkedList
  """
  @spec push(t, any()) :: t
  def push(linked_list, elem) do
    {list} = linked_list
    {[elem | list]}
  end

  @doc """
  Calculate the length of a LinkedList
  """
  @spec length(t) :: non_neg_integer()
  def length(linked_list) do
    {list} = linked_list
    Kernel.length(list)
  end

  @doc """
  Determine if a LinkedList is empty
  """
  @spec empty?(t) :: boolean()
  def empty?(linked_list) do
    {list} = linked_list
    list == []
  end

  @doc """
  Get the value of a head of the LinkedList
  """
  @spec peek(t) :: {:ok, any()} | {:error, :empty_list}
  def peek(linked_list) do
    {list} = linked_list
    case list do
      [] ->
        {:error, :empty_list}
      [head | _t] ->
        {:ok, head}
    end
  end

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail(linked_list) do
    {list} = linked_list
    case list do
      [] ->
        {:error, :empty_list}
      [_head | tail] ->
        {:ok, {tail}}
    end
  end

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop(linked_list) do
    {list} = linked_list
    case list do
      [] ->
        {:error, :empty_list}
      [head | tail] ->
        {:ok, head, {tail}}
    end
  end

  @doc """
  Construct a LinkedList from a stdlib List
  """
  @spec from_list(list()) :: t
  def from_list(list) do
    {list}
  end

  @doc """
  Construct a stdlib List LinkedList from a LinkedList
  """
  @spec to_list(t) :: list()
  def to_list(linked_list) do
    {list} = linked_list
    list
  end

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(linked_list) do
    {list} = linked_list
    rev_list = do_reverse(list, [])
    {rev_list}
  end

  defp do_reverse([], rev_list), do: rev_list
  defp do_reverse([head | tail], rev_list), do: do_reverse(tail, [head | rev_list])
end
