-module(sublist).

-export([is_equal/2, is_sublist/2, is_superlist/2, is_unequal/2, relation/2]).

is_equal(L1, L2) -> L1 == L2.

is_unequal(L1, L2) -> L1 /= L2.

is_sublist([], _L2) -> true;
is_sublist(_L1, []) -> false;
is_sublist(L1, L2) when length(L1) > length(L2) -> false;

is_sublist(L1 = [E1 | _ ], L2 = [ _ | Tail2]) -> 
    BeginMatchL2 = lists:dropwhile(fun (E) -> E /= E1 end, L2),
    lists:prefix(L1, BeginMatchL2) orelse is_sublist(L1, Tail2).

is_superlist([], []) -> true;
is_superlist(L1, L2) -> is_sublist(L2,L1).

relation(L1, L2) ->
    if 
        % test for sublist
        length(L1) < length(L2) ->
            unequal_or_relation(is_sublist(L1, L2), sublist);
        % test for superlist 
        length(L1) > length(L2) ->
            unequal_or_relation(is_superlist(L1, L2), superlist);
        % equal length
        true ->
           unequal_or_relation(L1 == L2, equal)
    end.

unequal_or_relation(false, _) -> unequal;
unequal_or_relation(true, Relation) -> Relation.
