-module(circular_buffer).

-export([cbuffer_loop/0,create/1, read/1, size/1, write/2, write_attempt/2]).

-spec create(pos_integer()) -> pid().
create(Size) ->
    Pid = spawn(fun circular_buffer:cbuffer_loop/0),
    Pid ! {create, Size},
    Pid.

-spec read(pid()) -> integer().
read(Pid) ->
  Pid ! {self(), read},
  receive
    {read_result, Result} ->
      Result
  end.

-spec size(pid()) -> pos_integer().
size(Pid) ->
  Pid ! {self(), size},
  receive
    {size_result, Size} ->
      Size
  end.

-spec write(pid(), integer()) -> ok.
write(Pid, Item) ->
  Pid ! {write, Item},
  ok.

-spec write_attempt(pid(), integer()) -> ok | {error, full}.
write_attempt(Pid, Item) ->
  Pid ! {self(), try_write, Item},
  receive
    {try_write_result, Result} ->
      Result
  end.

cbuffer_loop() ->
    receive
        {From, size} -> 
          From ! {size_result, {ok, get(size)}};
        {From, read} -> 
          From ! {read_result, do_read()};
        {From, try_write, Element} ->
          From ! {try_write_result, do_try_write(Element)};
        {write, Element} ->
            do_write(Element);
        {create, Size} ->
            put(head, 0),
            put(tail, 0),
            put(num_elements, 0),
            put(size, Size),
            put(buffer, array:new([{size, Size}, {fixed, true}]))
    end,
    cbuffer_loop().

do_read() ->
  NumElements = get(num_elements),
  if
    NumElements == 0 ->
      {error, empty};
    true ->
      Head = get(head),
      Array = get(buffer),
      Element = array:get(Head, Array),
      Size = get(size),
      put(num_elements, NumElements-1),
      put(head, (Head+1) rem Size),
      {ok, Element}
  end.

do_try_write(Element) ->
  NumElements = get(num_elements),
  Size = get(size),
  if
    NumElements == Size ->
      {error, full};
    true ->
      do_write(Element)
  end.

do_write(Element) ->
  NumElements = get(num_elements),
  Size = get(size),
  if
    NumElements == Size ->
      Head = get(head),
      put(head, (Head+1) rem Size);
    true ->
      put(num_elements, NumElements+1)
  end,
  Tail = get(tail),
  Array1 = get(buffer),
  Array2 = array:set(Tail, Element, Array1),
  put(buffer, Array2),
  put(tail, (Tail+1) rem Size),
  ok.
