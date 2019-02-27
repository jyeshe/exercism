-module(bob).

-export([response/1]).

is_question("") -> false;
is_question(String) -> lists:last(String) == $?.

is_yelling("") -> false;
is_yelling(String) ->
    MatchRes = re:run(String, "\\p{L}+"),
    String == string:uppercase(String) andalso MatchRes /= nomatch.

response(String) ->
    TrimedStr = string:trim(String),
    IsQuestion = is_question(TrimedStr),
    IsYelling = is_yelling(TrimedStr),
    if
        IsQuestion andalso IsYelling ->
            "Calm down, I know what I'm doing!";
        IsQuestion ->
            "Sure.";
        IsYelling ->
            "Whoa, chill out!";
        TrimedStr == "" ->
            "Fine. Be that way!";
        true ->
            "Whatever." 
    end.
