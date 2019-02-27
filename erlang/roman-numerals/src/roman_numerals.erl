-module(roman_numerals).

-export([roman/1]).

-spec roman(integer()) -> list().
roman(Number) -> roman_thousands(Number).

roman_sequence(Repeat_n, Letter) -> string:join(lists:duplicate(Repeat_n, Letter), "").

roman_thousands(Number) ->
    Thousands = Number div 1000,

    if 
        Thousands == 0 ->
          roman_hundreds(Number, "");
        true ->
          FirstRomanStr = roman_sequence(Thousands, "M"),
          roman_hundreds(Number - Thousands*1000, FirstRomanStr)
    end.

roman_hundreds(Number, RomanStr) ->
    Hundreds = Number div 100,
    SubtractedNumber = Number - Hundreds*100,
    New_RomanStr = add_roman(Hundreds, RomanStr, hundreds),
    roman_tens(SubtractedNumber, New_RomanStr).

roman_tens(Number, RomanStr) ->
    Tens = Number div 10,
    SubtractedNumber = Number - Tens*10,
    New_RomanStr = add_roman(Tens, RomanStr, tens),
    roman_units(SubtractedNumber, New_RomanStr).

roman_units(Number, RomanStr) ->
    Units = Number,
    add_roman(Units, RomanStr, units).

add_roman(Amount, RomanStr, Unit_type) ->

    {Unit, Fourth, Middle, Sixth, Nineth} = get_roman_letters(Unit_type),

    if
      Amount == 0 ->
        RomanStr;
      Amount < 4 ->
        RomanStr ++ roman_sequence(Amount, Unit);
      Amount == 4 ->
        RomanStr ++ Fourth;
      Amount == 5 ->
        RomanStr ++ Middle;
      Amount == 6 ->
        RomanStr ++ Sixth;
      Amount < 9 ->
        RomanStr ++ Middle ++ roman_sequence(Amount-5, Unit);
      Amount == 9 ->
        RomanStr ++ Nineth
    end.

get_roman_letters(Unit_type) ->
    case Unit_type of
      hundreds ->
        {"C", "CD", "D", "DC", "CM"};
      tens ->
        {"X", "XL", "L", "LX", "XC"};
      units ->
        {"I", "IV", "V", "VI", "IX"}
    end.