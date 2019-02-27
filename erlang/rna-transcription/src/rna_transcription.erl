-module(rna_transcription).

-export([to_rna/1]).

map_nucleotide(Nucleotide) ->
    case Nucleotide of
        $C -> $G;
        $G -> $C;
        $T -> $A;
        $A -> $U
    end.
    
to_rna(Strand) -> 
    [map_nucleotide(Nucleotide) || Nucleotide <- Strand ].
