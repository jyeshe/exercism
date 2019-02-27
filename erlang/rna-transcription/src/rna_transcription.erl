-module(rna_transcription).

-export([to_rna/1]).

map_nucleotide(Nucleotide) ->
    case Nucleotide of
        $C -> $G;
        $G -> $C;
        $T -> $A;
        $A -> $U
    end.

rna_sequence([], RnaSeq) -> RnaSeq;

rna_sequence([Nucleotide | Strand], RnaSeq) ->
    RnaComplement = map_nucleotide(Nucleotide),
    rna_sequence(Strand, RnaSeq ++ [RnaComplement]).
    
to_rna(Strand) -> 
    rna_sequence(Strand, []).
