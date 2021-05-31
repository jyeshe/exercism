-module(protein_translation).

-export([proteins/1]).

-spec proteins(iolist()) -> list(atom()).
proteins(Strand) -> protein_names(Strand, []).

protein_names([], NameList) -> list_names(NameList);
protein_names([P1, P2, P3 | Strand], NameList) -> 
    case name([P1, P2, P3]) of
        stop -> list_names(NameList);
        Name -> protein_names(Strand, [Name | NameList])
    end.

list_names(NameList) -> lists:reverse(NameList).

name("AUG") -> methionine;
name(Name) when Name == "UUU"; Name == "UUC" ->	phenylalanine;
name(Name) when Name == "UUA"; Name == "UUG" -> leucine;
name(Name) when Name == "UCU"; Name == "UCC"; Name == "UCA"; Name == "UCG" -> serine;
name(Name) when Name == "UAU"; Name == "UAC" -> tyrosine;
name(Name) when Name == "UGU"; Name == "UGC" -> cysteine;
name("UGG") -> tryptophan;
name(Name) when Name == "UAA"; Name == "UAG"; Name == "UGA" -> stop.
