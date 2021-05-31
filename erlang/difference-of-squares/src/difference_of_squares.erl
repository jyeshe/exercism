-module(difference_of_squares).

-export([difference_of_squares/1, square_of_sum/1, sum_of_squares/1]).

-spec difference_of_squares(non_neg_integer()) -> integer().
difference_of_squares(Number) -> square_of_sum(Number) - sum_of_squares(Number).

-spec square_of_sum(non_neg_integer()) -> non_neg_integer().
square_of_sum(Number) -> 
    Sum = sum_until(Number),
    Sum * Sum.

-spec sum_of_squares(non_neg_integer()) -> non_neg_integer().
sum_of_squares(Number) -> do_sum_of_squares(Number, 0).

%
% Internal
%
sum_until(Number) -> do_sum_until(Number, 0).

do_sum_until(0, Sum) -> Sum;
do_sum_until(Number, Sum) -> do_sum_until(Number-1, Sum + Number).

do_sum_of_squares(0, Sum) -> Sum;
do_sum_of_squares(Number, Sum) -> do_sum_of_squares(Number-1, Number*Number + Sum).