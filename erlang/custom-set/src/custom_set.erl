-module(custom_set).

-export([add/2, 
		 contains/2, 
		 difference/2, 
		 disjoint/2, 
		 empty/1, 
		 equal/2, 
		 from_list/1, 
		 intersection/2, 
		 subset/2,
	 	 union/2]).

-opaque set() :: #{type_key := custom_set}.
-export_type([set/0]).

%% API

-spec add(Element :: any(), set()) -> set().
add(Elem, Set) -> 
	maps:put(Elem, dummy, Set).

-spec contains(Element :: any(), set()) -> boolean().
contains(Elem, Set) -> 
	maps:find(Elem, Set) /= error.

-spec difference(Set1 :: set(), Set2 :: set()) -> set().
difference(Set1, Set2) -> 
	Map = maps:without(maps:keys(Set2), Set1),
	assure_set_type(Map).

-spec disjoint(Set1 :: set(), Set2 :: set()) -> boolean().
disjoint(Set1, Set2) -> 
	difference(Set1, Set2) == Set1.

-spec empty(set()) -> boolean().
empty(Set) -> 
	maps:size(Set) == 1.

-spec equal(Set1 :: set(), Set2 :: set()) -> boolean().
equal(Set1, Set2) -> 
	maps:size(Set1) == maps:size(Set2) andalso empty(difference(Set1, Set2)).

-spec from_list(list()) -> set().
from_list(List) -> 
	Set = assure_set_type(maps:new()),
	lists:foldl(fun (Elem, Acc) -> add(Elem, Acc) end, Set, List).

-spec intersection(Set1 :: set(), Set2 :: set()) -> set().
intersection(Set1, Set2) -> 
	difference(Set1, union(difference(Set1, Set2), difference(Set2, Set1))).

-spec subset(Set1 :: set(), Set2 :: set()) -> boolean().
subset(Set1, Set2) -> 
	Set = maps:without(maps:keys(Set1), Set2),
	maps:size(Set) == (maps:size(Set2) - maps:size(Set1)).

-spec union(Set1 :: set(), Set2 :: set()) -> set().
union(Set1, Set2) -> 
	maps:merge(Set1, Set2).

%% Internal 

-spec assure_set_type(map()) -> set().
assure_set_type(Map) ->
	maps:put(type_key, custom_set, Map).