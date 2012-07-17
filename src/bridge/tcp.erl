-module(bridge.tcp).
-export([send/2, connect/5, receive_data/3]).
-import(gen_tcp).
-import(erlang).
-import(binary).
-import(ssl).

%% Pseudo-transport layer (interfaces directly with TCP).

connect(Conn, Secure, Host, Port, Opts) ->
    if Secure ->
	    Mod = ssl;
       true ->
	    Mod = gen_tcp
    end,
    {ok, Sock} = Mod:connect(Host, Port, Opts),
    loop(Conn, Sock, fun Mod:send/2, []).

send(Sock, Msg) ->
    Sock ! {bridge, self(), Msg}.

receive_data(Conn, Buf, Data) ->
    Bin = list_to_binary(Buf ++ [Data]),
    if byte_size(Bin) > 4 ->
	    <<Len:32, Msg/binary>> = Bin,
	    if Len >= erlang:byte_size(Msg) ->
		    Conn ! {tcp, binary:part(Msg, 0, Len)},
		    [binary:part(Msg, Len, erlang:byte_size(Msg) - Len)];
	       true ->
		    [Msg]
	    end;
       true ->
	    [Bin]
    end.

loop(Conn, S, Send, Buf) ->
    receive
        {bridge, Conn, Data} ->
            Len = byte_size(Data),
            Send(S, <<Len:32, Data/binary>>),
            loop(Conn, S, Send, Buf);
        {tcp, S, Data} ->
            loop(Conn, S, Send, receive_data(Conn, Buf, Data));
        {ssl, S, Data} ->
            loop(Conn, S, Send, receive_data(Conn, Buf, Data));
        {tcp_closed, S} ->
            Conn ! disconnect,
            exit(normal);
        {ssl_closed, S} ->
            Conn ! disconnect,
            exit(normal);
        _Something ->
            .io:format("Unknown: ~p~n", [_Something]),
            .io:format("~p~n", [S]),
            loop(Conn, S, Send, Buf)
    end.
