-module(allergies).

-export([allergies/1, is_allergic_to/2]).

get_ref_scores() -> [{eggs, 1}, {peanuts, 2}, {shellfish, 4}, {strawberries, 8}, {tomatoes, 16}, {chocolate, 32}, {pollen, 64}, {cats, 128}].
get_norm_score(Score) -> Score rem 256.

allergies(Score) -> 
    NormScore = get_norm_score(Score),
    [PositiveAllergen || {PositiveAllergen, SubstScore} <- get_ref_scores(), SubstScore band NormScore == SubstScore].

is_allergic_to(Substance, Score) -> 
    NormScore = get_norm_score(Score),
    FindList = [PositiveAllergen || 
        {PositiveAllergen, SubstScore} <- get_ref_scores(), Substance == PositiveAllergen, SubstScore band NormScore == SubstScore],
    FindList /= [].
    