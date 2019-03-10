-module(transpose).

-export([transpose/1]).
% -export([map_lines_to_columns/2, transposed_line/2]).

transpose([]) -> [];

transpose(Lines) ->
    % transposed columns have the length of biggest line  
    MaxLineLen = 
        lists:foldl(
            fun (Line, MaxLen) ->
                if
                    length(Line) > MaxLen ->
                        length(Line);
                    true ->
                        MaxLen
                end
            end,
            0,
            Lines),
    % transforms each line into a column (an array)
    Columns = map_lines_to_columns(Lines, MaxLineLen),
    % concats the J element of every columns (e.g "ab" is the result for J=0, col1=["a","1"] and col2=["b","2"] )
    [ transposed_line(Columns, J) || J <- lists:seq(0, MaxLineLen-1) ].

% Returns a transposed line
transposed_line(Columns, J) ->
    % searches the last index to apply padding
    {_, LastChrIndex} = lists:foldl(
        fun (Column, {Index, MaxIndex}) ->
            ChrStr = array:get(J, Column),
            if
                ChrStr /= "" ->
                    {Index+1, Index};
                true ->
                    {Index+1, MaxIndex}
            end
        end,
        {0, 0}, Columns),
    % concats every J element applying padding when there is a char to the right
    {_, TLine} = lists:foldl(
        fun (Column, {Index, LineStr}) ->
            ChrStr = array:get(J, Column),
            if
                % empty and there is a char to the right
                ChrStr == "" andalso Index < LastChrIndex ->
                    % padds with whitespace
                    {Index+1, string:concat(LineStr, " ")};
                true ->
                    {Index+1, string:concat(LineStr, ChrStr)}
            end
        end,
        {0, ""}, Columns),
    TLine.

% Returns list of columns: each column is an array (e.g line "a1" becomes array with "a" and "1")
map_lines_to_columns(Lines, MaxLineLen) ->
    lists:foldl(
        fun (Line, ColumnsList) ->
            ColumnArrayEmpty = array:new([{size, MaxLineLen}, {default, ""}]),
            % transforms line "a1" into array with "a" and "1" elements
            {_, ColumnArray} = 
                lists:foldl(
                    fun (Chr, {IndexL, Column}) -> 
                        {IndexL+1, array:set(IndexL, [Chr], Column)} 
                    end, 
                    {0, ColumnArrayEmpty}, 
                    Line),
            ColumnsList ++ [ColumnArray]   
        end, 
        [], 
        Lines).
