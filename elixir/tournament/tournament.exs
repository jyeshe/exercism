defmodule Tournament do
  @doc """
  Given `input` lines representing two teams and whether the first of them won,
  lost, or reached a draw, separated by semicolons, calculate the statistics
  for each team's number of games played, won, drawn, lost, and total points
  for the season, and return a nicely-formatted string table.

  A win earns a team 3 points, a draw earns 1 point, and a loss earns nothing.

  Order the outcome by most total points for the season, and settle ties by
  listing the teams in alphabetical order.
  """
  @spec tally(input :: list(String.t())) :: String.t()
  def tally(input) do
    # prepare header and team field padding
    header = "Team                           | MP |  W |  D |  L |  P\n"
    team_pad_count = String.length("Team                           ")

    # gets tally map %{<<team>>: {mp, w, d, l, p}}
    final_tally =
      Enum.reduce(input, %{},
        fn match, tally ->
          # the match line tokens
          token_list = String.split(match, ";")

          # ignore lines without 3 tokens
          if length(token_list) != 3 do
            tally
          else
            [teamA | [teamB | [result]]] = token_list
            # update teamA stats
            tally_updated = update_team_stats(tally, teamA, result)
            # inverts result for teamB
            inverted_result = if result == "win", do: "loss", else: if result == "loss", do: "win"
            # update teamB stats
            update_team_stats(tally_updated, teamB, inverted_result || result)
          end
        end)

    # sorts teams by most points
    sorted_list = Enum.sort_by(final_tally, fn {team, stats} -> {_mp, _w, _d, _l, points} = stats; points end, &>=/2)
    # transform tally map into string
    tally_str =
      Enum.map(sorted_list,
        fn {team, {mplayed, wins, draws, losses, points}} ->
          "#{String.pad_trailing(team, team_pad_count, " ")}|#{pad_num(mplayed)} |#{pad_num(wins)} |#{pad_num(draws)} |#{pad_num(losses)} |#{pad_num(points)}"
        end)
      |> Enum.join("\n")
    # returns tally string with header
    header <> tally_str
  end

  defp pad_num(num), do: String.pad_leading(Integer.to_string(num), 3, " ")

  defp update_team_stats(tally, team, result) do
    team_stats = Map.get(tally, team)
    # gets new stats
    new_team_stats =
      if nil == team_stats do
        # first match of the team
        case result do
          "win" -> {1, 1, 0, 0, 3}
          "loss" -> {1, 0, 0, 1, 0}
          "draw" -> {1, 0, 1, 0, 1}
          _ -> nil
        end
      else
        {mplayed, wins, draws, losses, points} = team_stats

        case result do
          "win" -> {mplayed+1, wins+1, draws, losses, points+3}
          "loss" -> {mplayed+1, wins, draws, losses+1, points}
          "draw" -> {mplayed+1, wins, draws+1, losses, points+1}
          _ -> nil
        end
      end

    if new_team_stats != nil do
      Map.put(tally, team, new_team_stats)
    else
      tally
    end
  end
end
