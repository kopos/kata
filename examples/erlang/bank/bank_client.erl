-module(bank_client).
-export([deposit/2, withdraw/2, balance/1]).

-define(SERVER, "localhost").
-define(PORT, 3010).


deposit(Who, Amount) -> simple_rpc({deposit, Who, Amount}).
withdraw(Who, Amount) -> simple_rpc({withdraw, Who, Amount}).
balance(Who) -> simple_rpc({balance, Who}).


simple_rpc(X) ->
    case gen_tcp:connect(?SERVER, ?PORT, [binary, {packet, 4}]) of
        {ok, Socket} -> 
            gen_tcp:send(Socket, [term_to_binary(X)]),
            wait_reply(Socket);
        E ->
            E
    end.

wait_reply(Socket) ->
    receive 
        {tcp, Socket, Bin} ->
            Term = binary_to_term(Bin),
            gen_tcp:close(Socket),
            Term;
        {tcp_closed, Socket} ->
            true
    end.
