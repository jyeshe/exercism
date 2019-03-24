defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """
  use GenServer

  #
  # GenServer API
  #

  @doc """
  Creates a new GenServer process for one account.
  """
  def start_link(name) do
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  @doc """
  Initializes the account with a balance = 0.
  """
  def init(:ok) do
    {:ok, %{balance: 0}}
  end

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    registry = Process.whereis(BankAccount.Registry)

    if nil == registry do
      Registry.start_link(keys: :unique, name: BankAccount.Registry)
    end

    via_tuple = create_registry_via_tuple()

    {:ok, account_pid} = start_link(via_tuple)

    account_pid
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none | {:error, :account_closed}
  def close_bank(account) do
    # checks if open
    case account_exists?(account) do
      false ->
        {:error, :account_closed}
      _ ->
        GenServer.stop(account)
    end
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer | {:error, :account_closed}
  def balance(account) do
    # checks if open
    case account_exists?(account) do
      false ->
        {:error, :account_closed}
      _ ->
        GenServer.call(account, :balance)
    end
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    # checks if open
    case account_exists?(account) do
      false ->
        {:error, :account_closed}
      _ ->
        GenServer.call(account, {:update, amount})
    end
  end

  @doc """
  Handles :balance request.
  """
  def handle_call(:balance, _from, state), do: {:reply, state.balance, state}

  @doc """
  Handles :update request.
  """
  def handle_call({:update, amount}, _from, state) do
    abs_amount = abs(amount)
    cond do
      amount < 0 and state.balance < abs(abs_amount)->
        {:reply, {:error, :not_enough_balance}, state}
      true ->
        new_balance = state.balance + amount
        {:reply, {:ok, new_balance}, %{state | balance: new_balance}}
    end
  end

  ##
  ## Internal
  ##

  # Returns Registry :via tuple with a unique random name
  defp create_registry_via_tuple() do
    account_id = "BankAccount" <> Integer.to_string(Enum.random(1..1000000))
    reg_name_key = {BankAccount.Registry, account_id}

    {:via, Registry, reg_name_key}
  end

  # Checks if a pid is associated with an account
  defp account_exists?(account_pid) do
    [] != Registry.keys(BankAccount.Registry, account_pid)
  end
end
