-module(luhn).

-export([valid/1,checksum/3]).

valid(NumberSeq) -> 
    Sum = checksum(lists:reverse(NumberSeq), 1, 0),
    Sum rem 10 == 0.

% single and empty input
checksum([], Index, _Sum) when Index == 1; Index == 2-> -1;

% base case
checksum([], _Index, Sum) -> Sum;

% ignore whitespace (32)
checksum([DigitChr | OtherDigits], Index, Sum) when DigitChr == 32 -> 
    checksum(OtherDigits, Index, Sum);

% digits loop
checksum([DigitChr | OtherDigits], Index, Sum) when DigitChr >= $0, DigitChr =< $9->
    Doubled = double_second(DigitChr - $0, Index),
    checksum(OtherDigits, Index+1, Sum+Doubled);

% invalid input
checksum(_Digits, _Index, _Sum) -> -1.

double_second(Digit, Index)  when Index rem 2 /= 0 -> Digit;
double_second(Digit, _Index) when Digit > 4 -> 2 * Digit - 9;
double_second(Digit, _Index) -> 2 * Digit.