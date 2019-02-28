-module(series).

-export([from_string/2, test_version/0]).

from_string(Width, String) ->
  series(Width, String, []).

%% Internal

series(Width, String, Series) when length(String) < Width ->
  Series; 

series(Width, String = [_Chr | StrTail], Series) ->
  SubList = lists:sublist(String, 1, Width),
  series(Width, StrTail, Series ++ [SubList]).

test_version() -> 1.
