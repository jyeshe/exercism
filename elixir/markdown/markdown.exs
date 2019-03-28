defmodule Markdown do
  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    ## Examples

    iex> Markdown.parse("This is a paragraph")
    "<p>This is a paragraph</p>"

    iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
    "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"
  """
  @spec parse(String.t()) :: String.t()
  def parse(input) do
    String.split(input, "\n")
      |> Enum.map(&(process_md_line/1))
      |> Enum.join()
      |> enclose_tag_ulist()
  end

  defp process_md_line(md_text) do
    first_char = String.first(md_text)

    cond do
      first_char == "#" ->
        md_text
          |> parse_md_header()
          |> enclose_tag_header()

      first_char == "*" ->
        md_text
          |> parse_md_list()
          |> enclose_tag_list()

      true ->
        enclose_tag_paragraph(md_text)
    end
  end

  defp parse_md_header(md_header) do
    [sharp | text] = String.split(md_header, " ", parts: 2)
    { Integer.to_string(String.length(sharp)), hd text }
  end

  defp parse_md_list(md_list) do
    md_list
      |> String.trim_leading("* ")
      |> String.split()
  end

  defp enclose_tag_header({num, text}) do
    "<h" <> num <> ">" <> text <> "</h" <> num <> ">"
  end

  defp enclose_tag_list(md_words) do
    "<li>" <> join_words_with_tags(md_words) <> "</li>"
  end

  defp enclose_tag_paragraph(md_text) do
    # same style on enclosing (concat more legible than interpolation)
    "<p>" <> join_words_with_tags(String.split(md_text)) <> "</p>"
  end

  defp join_words_with_tags(md_words) do
    md_words
      |> Enum.map(&(replace_md_with_tag/1))
      |> Enum.join(" ")
  end

  defp replace_md_with_tag(md_word) do
    cond do
      md_word =~ ~r/^\_{2}/ ->
        md_word
          |> replace_once("__", "<strong>")
          |> String.replace_trailing("__", "</strong>")
      md_word =~ ~r/\_{2}$/ ->
        md_word
          |> String.replace_trailing("__", "</strong>")
      md_word =~ ~r/^\_/ ->
        md_word
          |> replace_once("_", "<em>")
          |> String.replace_trailing("_", "</em>")
      md_word =~ ~r/\_$/ ->
        md_word
          |> String.replace_trailing("_", "</em>")
      true ->
        md_word
    end
  end

  defp replace_once(in_str,this,with_this), do: String.replace(in_str, this, with_this, global: false)

  defp enclose_tag_ulist(html) do
    html
      |> replace_once("<li>", "<ul><li>")
      |> String.replace_trailing("</li>", "</li></ul>")
  end
end
