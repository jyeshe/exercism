defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l) do
    do_count(l, 0)
  end

  defp do_count([], counter), do: counter
  defp do_count([_h | t], counter) do
    do_count(t, counter+1)
  end

  @spec reverse(list) :: list
  def reverse(l) do
    do_reverse(l, [])
  end

  defp do_reverse([], reversed), do: reversed
  defp do_reverse([h | t], reversed) do
    do_reverse(t, [h | reversed])
  end

  @spec map(list, (any -> any)) :: list
  def map(l, f) do
    do_map(l, f, [])
  end

  defp do_map([], _f, output_list), do: reverse(output_list)
  defp do_map([h | t], func, output_list) do
    do_map(t, func, [func.(h) | output_list])
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f) do
    do_filter(l, f, [])
  end

  defp do_filter([], _f, output_list), do: reverse(output_list)

  defp do_filter([h | t], func, output_list) do
    if func.(h) do
      do_filter(t, func, [h | output_list])
    else
      do_filter(t, func, output_list)
    end
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, _f), do: acc
  def reduce([h | t], acc, func) do
    reduce(t, func.(h, acc), func)
  end

  @spec append(list, list) :: list
  def append(a, b) do
    do_append(a, b, [])
  end

  defp do_append(a, [], []), do: a

  defp do_append([], [], output_list), do: reverse(output_list)

  defp do_append([], [h | t], output_list) do
    do_append([], t, [h | output_list])
  end

  defp do_append([h | t], l2, output_list) do
    do_append(t, l2, [h | output_list])
  end

  @spec concat([[any]]) :: [any]
  def concat(ll) do
    do_concat(ll, [], [])
  end

  defp do_concat([], [], output_list), do: reverse(output_list)

  defp do_concat([list1 | other_lists], [], output_list) do
    do_concat(other_lists, list1, output_list)
  end

  defp do_concat(other_lists, [h | t], output_list) do
    do_concat(other_lists, t, [h | output_list])
  end
end
