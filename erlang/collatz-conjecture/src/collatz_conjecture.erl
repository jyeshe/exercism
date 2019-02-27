-module(collatz_conjecture).

-export([steps/1]).

collatz(N, _Acc) when N =< 0 -> 
    {error, "Only positive numbers are allowed"};

collatz(1, Acc) -> Acc;

collatz(PairNum, Acc) when PairNum rem 2 == 0 -> 
    collatz(PairNum div 2, Acc+1);

collatz(N, Acc) -> 
    collatz(3*N+1, Acc+1).

steps(N) -> 
    collatz(N, 0).
