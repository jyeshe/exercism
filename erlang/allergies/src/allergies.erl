-module(allergies).

-export([allergies/1, is_allergic_to/2]).

get_ref_scores() -> [{eggs, 1}, {peanuts, 2}, {shellfish, 4}, {strawberries, 8}, {tomatoes, 16}, {chocolate, 32}, {pollen, 64}, {cats, 128}].
get_norm_score(Score) -> Score rem 256.

allergies(Score) -> 
    NormScore = get_norm_score(Score),
    RefScores = get_ref_scores(),
    AddPositiveFun = fun ({K,V}, PositiveList) -> 
            if 
                (NormScore band V) == V ->
                    [K | PositiveList];
                true ->
                    PositiveList
            end
        end,
    lists:foldl(AddPositiveFun, [], RefScores).


is_allergic_to(Substance, Score) -> 
    NormScore = get_norm_score(Score),
    RefScores = get_ref_scores(),
    FindRes = lists:keyfind(Substance, 1, RefScores),

    if
        FindRes == false ->
            undefined;
        true ->
            {_K, SubstScore} = FindRes,
            (NormScore band SubstScore) == SubstScore
    end.
