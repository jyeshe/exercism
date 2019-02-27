-module(raindrops).

-export([convert/1]).

convert(Number) -> raindrop_speak(Number).

raindrop(Num, 3) when Num rem 3 == 0 -> "Pling";
raindrop(_Num, 3) -> "";

raindrop(Num, 5) when Num rem 5 == 0 -> "Plang";
raindrop(_Num, 5) -> "";

raindrop(Num, 7) when Num rem 7 == 0 -> "Plong";
raindrop(_Num, 7) -> "".

raindrop_speak(Num) -> 
    Speak = raindrop(Num, 3) ++ raindrop(Num, 5) ++ raindrop(Num, 7),
    if
        Speak /= "" -> Speak;
        true -> integer_to_list(Num)
    end.
