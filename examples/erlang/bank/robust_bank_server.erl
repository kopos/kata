-module(robust_bank_server).
-export([bstart/1, start/1, stop/1]).

-include("reply.hrl").

bstart([APort]) ->
    Port = list_to_integer(atom_to_list(APort)),
    start(Port).


start(Port) ->
    mnesia:start(),
    spawn_link(fun() -> server(Port) end).

stop(Port) ->
    mnesia:stop(),
    tcp_server:stop(Port).

server(Port) ->
    tcp_server:start_raw_server(Port, 
                                fun(Socket) -> input_handler(Socket, Port) end,
                                15,
                                4).

input_handler(Socket, Port) ->
    receive
        {tcp, Socket, Bin} ->
            case binary_to_term(Bin) of
                {call, Tag, Term} ->
                    io:format("Server port:~p Tag:~p call:~p~n", [Port, Tag, Term]),
                    Reply = do_call(Tag, Term),
                    send_term(Socket, {ack, Tag, Reply}),
                    input_handler(Socket, Port);
                {delete_tag, Tag} ->
                    io:format("Server port:~p deleteTag:~p~n", [Port, Tag]),
                    delete_tag(Tag)
            end;
        {tcp_closed, Socket} ->
            true
    end.

send_term(Socket, Term) ->
    gen_tcp:send(Socket, term_to_binary(Term)).

do_call(Tag, C) ->
    Fun = the_func(C),
    F = fun() ->
            case mnesia:reply({reply, Tag}) of
                [] ->
                    Val = Fun(),
                    mnesia:write(#reply{tag=Tag, val=Val}),
                    Val;
                [C] ->
                    C#reply.val
            end
        end,
    mnesia:transaction(F).

the_func({deposit, Who, Amount}) -> bank:deposit(Who, Amount);
the_func({withdraw, Who, Amount}) -> bank:withdraw(Who, Amount);
the_func({balance, Who}) -> bank:balance(Who).

delete_tag(Tag) ->
    F = fun() -> mnesia:delete({reply, Tag}) end,
    V = mnesia:transaction(F),
    io:format("delete tag=~p~n", [V]).
