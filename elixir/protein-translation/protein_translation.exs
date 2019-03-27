defmodule ProteinTranslation do

  @codon_protein_map %{
    "UGU" => "Cysteine",
    "UGC" => "Cysteine",
    "UUA" => "Leucine",
    "UUG" => "Leucine",
    "AUG" => "Methionine",
    "UUU" => "Phenylalanine",
    "UUC" => "Phenylalanine",
    "UCU" => "Serine",
    "UCC" => "Serine",
    "UCA" => "Serine",
    "UCG" => "Serine",
    "UGG" => "Tryptophan",
    "UAU" => "Tyrosine",
    "UAC" => "Tyrosine",
    "UAA" => "STOP",
    "UAG" => "STOP",
    "UGA" => "STOP"
  }

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: {atom, list(String.t())}
  def of_rna(rna) do
    # splits rna sequence into list of codons (each codon being a list of 3 graphemes)
    codons = String.splitter(rna, "", trim: true) |> Enum.chunk_every(3)

    proteins = Enum.reduce_while(codons, [],
      fn codon_in_list, acc ->
        codon = Enum.join(codon_in_list) # e.g ["U", "A", "G"] => "UAG"
        protein = @codon_protein_map[codon]

        cond do
          protein != "STOP" ->
            # valid protein
            {:cont, acc ++ [protein]}
          protein == nil ->
            # invalidates rna sequence
            {:halt, [nil]}
          true ->
            # == "STOP"
            {:halt, acc}
        end
      end)

    case Enum.find_index(proteins, &(&1 == nil)) do
      nil -> # index nil so did not find
        {:ok, proteins}
      _ ->
        {:error, "invalid RNA"}
    end
  end

  @doc """
  Given a codon, return the corresponding protein
  """
  @spec of_codon(String.t()) :: {atom, String.t()}
  def of_codon(codon) do
    case @codon_protein_map[codon] do
      nil ->
        {:error, "invalid codon"}
      protein ->
        {:ok, protein}
    end
  end
end
