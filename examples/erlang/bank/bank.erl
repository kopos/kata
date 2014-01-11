-module(bank).
-export([deposit/2, withdraw/2, balance/1]).

-include("bank.hrl").

deposit(Who, Amount) ->
    fun() ->
        case mnesia:read({account, Who}) of
            [] ->
                Entry = #account{name=Who, balance=Amount},
                mnesia:write(Entry),
                Amount;
            [E] ->
                Old = E#account.balance,
                New = Old + Amount,
                E1 = E#account{balance=New},
                mnesia:write(E1),
                New
        end
    end.


balance(Who) ->
    fun() ->
        case mnesia:read({account, Who}) of
            [] ->
                {error, no_such_account};
            [E] ->
                B = E#account.balance,
                {ok, B}
        end
    end.

withdraw(Who, Amount) ->
    fun() ->
        case mnesia:read({account, Who}) of
            [] ->
                {error, no_such_account};
            [E] ->
                Old = E#account.balance,
                if 
                    Old >= Amount ->
                        New = Old - Amount,
                        E1 = E#account{balance=New},
                        mnesia:write(E1),
                        ok;
                    Old < Amount ->
                        {error, not_enough_money}
                end
        end
    end.
