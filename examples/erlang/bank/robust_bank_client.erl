-module(robust_bank_client).
-export([deposit/2, withdraw/2, balance/1]).

-define(SERVER, "localhost").
-define(PORT, 3010).
-define(PORT1, 3020).
-define(PORT2, 3030).


deposit(Who, Amount) -> robust_rpc({deposit, Who, Amount}).
withdraw(Who, Amount) -> robust_rpc({withdraw, Who, Amount}).
balance(Who) -> robust_rpc({balance, Who}).


robust_rpc(Amount) ->
    Id = make_ref(),
    Amount1 = {call, Id, Amount},
    io:format("Trying to connect to server 1~n"),
    case gen_tcp:connect(?SERVER, ?PORT1, [binary, {packet, 4}]) of
        {ok, Socket} -> 
            io:format("Sending to server 1~n"),
            gen_tcp:send(Socket, [term_to_binary(Amount1)]),
            wait_reply1(Socket, Id, Amount);
        {error, _} ->
            io:format("Cannot connect to server 1~n"),
            robust_rpc_try_again(Id, Amount)
    end.

wait_reply1(Socket, Id, Amount) ->
    receive 
        {tcp, Socket, Bin} ->
            case binary_to_term(Bin) of
                {ack, Id, Reply} ->
                    io:format("Server 1 replied~n"),
                    B = term_to_binary({delete_tag, Id}),
                    gen_tcp:send(Socket, B),
                    gen_tcp:close(Socket),
                    {ok, {server1, Reply}};
                _ ->
                    robust_rpc_try_again(Id, Amount)
            end;
        {tcp_closed, Socket} ->
            robust_rpc_try_again(Id, Amount)
    after 10000 ->
        io:format("timeout from server 1~n"),
        gen_tcp:close(Socket),
        robust_rpc_try_again(Id, Amount)
    end.


robust_rpc_try_again(Id, Amount) ->
    io:format("Trying to connect to server 2~n"),
    case gen_tcp:connect(?SERVER, ?PORT2, [binary, {packet, 4}]) of
        {ok, Socket} ->
            Amount1 = {call, Id, Amount},
            io:format("Sending to server 2~n"),
            gen_tcp:send(Socket, [term_to_binary(Amount1)]),
            wait_reply2(Socket, Id);
        {error, _} ->
            io:format("Cannot connect to server 2~n"),
            {error, both_servers_down}
    end.

wait_reply2(Socket, Id) ->
    receive
        {tcp, Socket, Bin} ->
            case binary_to_term(Bin) of
                {ack, Id, Reply} ->
                    B = term_to_binary({delete_tag, Id}),
                    gen_tcp:send(Socket, B),
                    gen_tcp:close(Socket),
                    {ok, {server2, Reply}};
                0 ->
                    {error, unexpected_reply, 0}
            end;
        {tcp_closed, socket} ->
            {error, server2}
    after 10000 ->
        io:format("timeout from server 2~n"),
        gen_tcp:close(Socket),
        {error, no_reply_server2}
    end.
