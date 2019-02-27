-module(clock).

-export([create/2, is_equal/2, minutes_add/2, to_string/1]).

-record(clock, {time = 0}).

create(Hour, Minute) ->
    % converts everything in minutes
    #clock{time = normalize(Hour * 60 + Minute)}.

is_equal(Clock1, Clock2) -> Clock1 == Clock2.

minutes_add(#clock{time = Minutes}, AdditionalMinutes) -> 
    #clock{time = normalize(Minutes + AdditionalMinutes) }.

to_string(#clock{time = Minutes}) -> 
    lists:flatten(io_lib:format("~2..0B:~2..0B", split(Minutes))).

split(Minutes) ->
    [Minutes div 60, Minutes rem 60].

normalize(Minutes) when Minutes < 0 ->
    % intra day framed time 
    1440 + (Minutes rem 1440);

normalize(Minutes) -> Minutes rem 1440.