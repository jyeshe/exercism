duplicates =
  System.argv()
  |> List.first()
  |> String.to_integer()

text = "The quick brown fox jumps over the lazy dog"
texts = List.duplicate(text, duplicates)

Benchee.run(%{
  "1 worker" => fn -> Frequency.frequency(texts, 1) end,
  "2 workers" => fn -> Frequency.frequency(texts, 2) end,
  "4 workers" => fn -> Frequency.frequency(texts, 4) end,
  "8 workers" => fn -> Frequency.frequency(texts, 8) end,
})
