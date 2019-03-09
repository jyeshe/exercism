-module(change).

-export([find_fewest_coins/2]).

find_fewest_coins(Amount, Coins) ->
	% init best change length with huge number of factors
	put(best, 999999999),
	% starts with higher coins
	RevCoins = lists:reverse(Coins),
	% avoids getting the last on reversed list
	NoUnitary = hd(Coins) /= 1,
	give_change(Amount, RevCoins, NoUnitary, []).

% Amount = 0
give_change(0, _, _, _) -> [];

% Invalid Input
give_change(AmountLeft, _Coins, _NoUnitary, _Change) when AmountLeft < 0 ->
	{error, invalid_target_value};

% Uses maximum of higher coins
give_change(AmountLeft, [CoinI | CoinsTail], NoUnitary, Change) ->
	NumCoinI = AmountLeft div CoinI,
	Remainder = AmountLeft rem CoinI,
	SmallestLen = get(best),

	if
		% Avoids continuing with unecessary search
		length(Change) >= SmallestLen ->
			undefined;
		% CoinI is the last coin and target not achieved 
		Remainder /= 0 andalso CoinsTail == [] ->
			undefined;
		% CoinI is not part of the change
		NumCoinI == 0 ->
			give_change(AmountLeft, CoinsTail, NoUnitary, Change);
		% CoinI finishes the change
		Remainder == 0 ->
			CompleteChange = Change ++ lists:duplicate(NumCoinI, CoinI),
			if 
				length(CompleteChange) < SmallestLen ->
					% if it's the best change, share the length with other searches
					put(best, length(CompleteChange)),
					CompleteChange;
				true ->
					% discards
					undefined
			end;
		% Remainder /= 0 and NoUnitary and more coins to try
		NoUnitary ->
			% generates combinatory tree spliting processing into NumCoinI+1 ways: change with full, partial uses of CoinI or no use of CoinI
			AllUsesOfCoinI = [
				give_change(AmountLeft-(NumCoinI-J)*CoinI, CoinsTail, NoUnitary, Change++lists:duplicate(NumCoinI-J, CoinI))
				||
				J <- lists:seq(0, NumCoinI)
			],
			lists:foldl(fun (L, Smaller) -> fewer_coins(L, Smaller) end, undefined, AllUsesOfCoinI);
		% Remainder /= 0
		true ->
			% generates combinatory tree spliting processing into two ways: change with and without CoinI
			fewer_coins(
				give_change(Remainder, CoinsTail, NoUnitary, Change ++ lists:duplicate(NumCoinI, CoinI)), 
				give_change(AmountLeft, CoinsTail, NoUnitary, Change))
	end.

fewer_coins(L1, L2) ->
	if
		L2 == undefined orelse length(L1) < length(L2) ->
			L1;
		true ->
			L2
	end.
