-module(sublist).

-export([is_equal/2, is_sublist/2, is_superlist/2, is_unequal/2, relation/2]).

is_equal(L1, L2) -> L1 == L2.

is_unequal(L1, L2) -> L1 /= L2.

is_sublist([], _L2) -> true;
is_sublist(_L1, []) -> false;
is_sublist(L1 = [E1 | _Tail1], L2 = [_E2 | Tail2]) -> 
    if
        length(L1) > length(L2) ->
            false;
        true ->
            BeginMatchL2 = lists:dropwhile(fun (E) -> E /= E1 end, L2),
            lists:prefix(L1, BeginMatchL2) orelse is_sublist(L1, Tail2)
    end.

is_superlist([], []) -> true;
is_superlist(L1, L2) -> is_sublist(L2,L1).

relation_or_unequal(Bool, Relation) ->
    if
        not Bool -> 
            unequal;
        true -> 
            Relation
    end.

relation(L1, L2) -> 
    if 
        % test for sublist
        length(L1) < length(L2) ->
            relation_or_unequal(is_sublist(L1, L2), sublist);
        % test for superlist 
        length(L1) > length(L2) ->
            relation_or_unequal(is_superlist(L1, L2), superlist);
        % equal length
        true ->
           relation_or_unequal(L1 == L2, equal)
    end.