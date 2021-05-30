-module(nucleotide_count).

-export([count/2, nucleotide_counts/1]).

-define(IS_INVALID(N), N /= $A andalso N /= $C andalso N /= $G andalso N /= $T).

count([], _Nucleotide) -> 0;
count(Strand, [Nucleotide | _]) -> do_count(Strand, Nucleotide, 0).

do_count([], _Nucleotide, Counter) -> Counter;
do_count([N | _Strand], _Nucleotide, _Counter) when ?IS_INVALID(N) -> erlang:error(bad_nucleotide);
do_count([N | Strand], Nucleotide, Counter) when N == Nucleotide -> do_count(Strand, Nucleotide, Counter+1);
do_count([_N | Strand], Nucleotide, Counter) -> do_count(Strand, Nucleotide, Counter).

nucleotide_counts(Strand) -> 
    [
        {"A", count(Strand, "A")}, 
        {"C", count(Strand, "C")}, 
        {"G", count(Strand, "G")}, 
        {"T", count(Strand, "T")}
    ].


