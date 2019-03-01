-module(change).

-export([find_fewest_coins/2]).

find_fewest_coins(Amount, Coins) ->
	% starts with higher coins
	RevCoins = lists:reverse(Coins),
	NoUnitary = hd(Coins) /= 1,
	give_change(Amount, Amount, RevCoins, RevCoins, NoUnitary, []).

% Invalid Input
give_change(_AmountLeft, WholeAmount, _Coins, _AllCoins, _NoUnitary, _Change) when WholeAmount < 0 -> 
	{error, invalid_target_value};

% Base Case for Change with partial use of one or more Coins
give_change(0, _WholeAmount, _AnyCoins, _AllCoins, _NoUnitary, Change) -> 
	Change;

% Uses maximum of higher coins
give_change(AmountLeft, WholeAmount, [CoinI | CoinsTail], AllCoins, NoUnitary, Change) ->
	% io:fwrite("~nAmount: ~B, CoinI: ~B, Change: ~0p~n", [AmountLeft, CoinI, Change]),
	NumCoinI = AmountLeft div CoinI,
	Remainder = AmountLeft rem CoinI,
	ChangeWithCoinI = Change ++ lists:duplicate(NumCoinI, CoinI),

	if
		% CoinI is the last coin and target not achieved 
		Remainder /= 0 andalso CoinsTail == [] ->
			try_begin_smaller_coins(WholeAmount, tl(AllCoins), NoUnitary);
		% CoinI is not part of the change
		NumCoinI == 0 ->
			give_change(AmountLeft, WholeAmount, CoinsTail, AllCoins, NoUnitary, Change);
		% CoinI finishes the change
		Remainder == 0 ->
			ChangeWithCoinI;
		% Remainder /= 0 and NoUnitary and more coins to try
		NoUnitary ->
			% generates combinatory tree spliting processing into NumCoinI+1 ways: change with full, partial uses of CoinI or no use of CoinI
			AllUsesOfCoinI = [
				give_change(AmountLeft-(NumCoinI-J)*CoinI, WholeAmount, CoinsTail, AllCoins, NoUnitary, Change++lists:duplicate(NumCoinI-J, CoinI))
				||
				J <- lists:seq(0, NumCoinI)
			],
			lists:foldl(fun (L, Smaller) -> fewer_coins(L, Smaller) end, undefined, AllUsesOfCoinI);
		% Remainder /= 0
		true ->
			% generates combinatory tree spliting processing into two ways: change with and without CoinI
			fewer_coins(
				give_change(Remainder, WholeAmount, CoinsTail, AllCoins, NoUnitary, ChangeWithCoinI), 
				give_change(AmountLeft, WholeAmount, CoinsTail, AllCoins, NoUnitary, Change))
	end.

try_begin_smaller_coins(_WholeAmount, [], _NoUnitary) ->
	undefined;

try_begin_smaller_coins(WholeAmount, SmallerCoins, NoUnitary) ->
	give_change(WholeAmount, WholeAmount, SmallerCoins, SmallerCoins, NoUnitary, []).

fewer_coins(L1, L2) ->
	if
		L2 == undefined orelse length(L1) < length(L2) ->
			L1;
		true ->
			L2
	end.
