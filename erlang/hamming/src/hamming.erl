-module(hamming).

-export([distance/2]).

update_counter(Letter1, Letter2, DiffCount) ->
    if 
        Letter1 == Letter2 -> 
            DiffCount;
        true ->
            DiffCount+1
    end.

calc_distance([], [], DiffCount) -> DiffCount;
calc_distance([Letter1 | Strand1], [Letter2 | Strand2], DiffCount) -> 
    NewDiffCount = update_counter(Letter1, Letter2, DiffCount),
    calc_distance(Strand1, Strand2, NewDiffCount).

distance(Strand1, Strand2) ->
    if 
        length(Strand2) /= length(Strand1) -> 
            {error, "left and right strands must be of equal length"};
        true ->
            calc_distance(Strand2, Strand1, 0)
    end.

