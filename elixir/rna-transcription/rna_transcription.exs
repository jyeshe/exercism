defmodule RNATranscription do
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RNATranscription.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    for c <- dna do
      case c do
        ?C -> ?G
        ?G -> ?C
        ?A -> ?U
        ?T -> ?A
      end
    end
  end
end
