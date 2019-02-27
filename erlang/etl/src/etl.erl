-module(etl).

-export([transform/1]).

transform(Old) -> 
    [{string:lowercase(Letter), Score} || {Score, LetterList} <- Old, Letter <- LetterList].
