-module(word_count).

-export([count_words/1, get_words_list/1]).

get_words_list(nomatch) -> [];
get_words_list({match, List}) ->
    lists:map(fun ([Word]) -> string:to_lower(Word) end, List).

count_words(Sentence) -> 
    MatchRes = re:run(Sentence, "\\p{L}+\'*\\p{L}+|\\p{Nd}+", [unicode, global, {capture, all, list}]),
    WordsList = get_words_list(MatchRes),
    FunCount = fun(N) -> N + 1 end,
    lists:foldl(
        fun (Elem, AccMap) ->
            maps:update_with(Elem,FunCount,1,AccMap) 
        end, 
        #{}, WordsList).
