-module(bank_account).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).

-export([balance/1, charge/2, close/1, create/0, deposit/2, withdraw/2]).

-record(account, {balance = 0}).

%%
%% exercism
%%
-spec balance(pid()) -> non_neg_integer() | {error, account_closed}.
balance(Pid) ->
    case is_process_alive(Pid) of 
        false ->
            {error, account_closed};
        true ->
            gen_server:call(Pid, {balance})
    end.

-spec charge(pid(), non_neg_integer()) -> non_neg_integer().
charge(_Pid, Amount) when Amount < 0 ->
    0;
charge(Pid, Amount) ->
  gen_server:call(Pid, {charge, Amount}).

-spec close(pid()) -> non_neg_integer() | {error, account_closed}.
close(Pid) ->
    case balance(Pid) of
        {error, _} = Error ->
            Error;
        Balance ->
            ok = gen_server:stop(Pid),
            Balance
    end.

-spec create() -> pid(). 
create() ->
  {ok, Pid} = gen_server:start(?MODULE, [], []),
  Pid.

-spec deposit(pid(), non_neg_integer()) -> boolean(). 
deposit(Pid, Amount) ->
    Amount > 0 andalso gen_server:cast(Pid, {deposit, Amount}) == ok.

-spec withdraw(pid(), non_neg_integer()) -> integer().
withdraw(_Pid, Amount) when Amount =< 0 -> 0;
withdraw(Pid, Amount)->
    gen_server:call(Pid, {withdraw, Amount}).

%%
%% gen_server callbacks 
%%
init([]) ->
    {ok, #account{}}.

handle_call({balance}, _From, State = #account{balance = Balance}) ->
    {reply, Balance, State};

handle_call({withdraw, Amount}, _From, #account{balance = Balance}) ->
    {Withdraw, NewBalance} = do_withdraw(Amount, Balance),
    {reply, Withdraw, #account{balance = NewBalance}};

handle_call({charge, Amount}, _From, #account{balance = Balance}) ->
    {Charge, NewBalance} = do_charge(Amount, Balance),
    {reply, Charge, #account{balance = NewBalance}}.

handle_cast({deposit, Amount}, #account{balance = Balance}) ->
    {noreply, #account{balance = Balance + Amount}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal
%%
% do_withdraw() -> {Withdraw, NewBalance :: non_neg_integer()} 
do_withdraw(Amount, Balance) when Amount > Balance ->
    {Balance, 0};
do_withdraw(Amount, Balance) ->
    {Amount, Balance-Amount}.

% do_charge() -> {Charge, NewBalance:: non_neg_integer()}
do_charge(Amount, Balance) when Amount > Balance ->
    {0, Balance};
do_charge(Amount, Balance) ->
    {Amount, Balance-Amount}.