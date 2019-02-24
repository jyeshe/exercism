-module(custom_set).

-export([add/2, contains/2, difference/2, disjoint/2, empty/1, equal/2, from_list/1, intersection/2, subset/2,
	 union/2]).

-type custom_set() :: list().

-spec add(Element :: any(), custom_set()) -> custom_set().
add(Elem, Set) -> 
	Contains = contains(Elem, Set),
	if
		Contains -> Set;
		true -> [Elem | Set]
	end.

-spec contains(Element :: any(), custom_set()) -> boolean().
contains(Elem, Set) -> 
	lists:member(Elem, Set).

-spec difference(Set1 :: custom_set(), Set2 :: custom_set()) -> custom_set().
difference(Set1, Set2) -> 
	Set1 -- Set2.

-spec disjoint(Set1 :: custom_set(), Set2 :: custom_set()) -> boolean().
disjoint(Set1, Set2) -> 
	difference(Set1, Set2) == Set1.

-spec empty(custom_set()) -> boolean().
empty(Set) -> 
	Set == [].

-spec equal(Set1 :: custom_set(), Set2 :: custom_set()) -> boolean().
equal(Set1, Set2) -> 
	length(Set1) == length(Set2) andalso lists:sort(Set1) == lists:sort(Set2).

-spec from_list(list()) -> custom_set().
from_list(List) -> 
	add_elements(List, []).

-spec intersection(Set1 :: custom_set(), Set2 :: custom_set()) -> custom_set().
intersection(Set1, Set2) -> 
	difference(Set1, union(difference(Set1, Set2), difference(Set2, Set1))).

-spec subset(Set1 :: custom_set(), Set2 :: custom_set()) -> custom_set().
subset(Set1, Set2) -> 
	difference(Set1, Set2) == [].

-spec union(Set1 :: custom_set(), Set2 :: custom_set()) -> custom_set().
union(Set1, Set2) -> 
	add_elements(Set1, Set2).

-spec add_elements(list(), Set :: custom_set()) -> custom_set().
add_elements(List, Set) -> 
	lists:foldl(fun (Elem, Acc) -> add(Elem, Acc) end, Set, List).