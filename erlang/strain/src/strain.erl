-module(strain).

-export([keep/2, discard/2]).
  
filter_item(PredTrue, FilteredList, Item) ->
    if
      PredTrue -> FilteredList ++ [Item];
      true -> FilteredList
    end.

keep_output(_Fn, [], FilteredList) -> FilteredList;

keep_output(Fn, [Item | List], FilteredList) ->
  NewFiltered = filter_item(Fn(Item), FilteredList, Item),
  keep_output(Fn, List, NewFiltered).

keep(Fn, List) -> % lists:filter
  keep_output(Fn, List, []).

discard(Fn, List) ->
  List -- keep(Fn, List).
