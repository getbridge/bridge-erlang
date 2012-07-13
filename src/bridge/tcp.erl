-module(bridge.tcp).
-export([send/2, connect/4, receive_data/3]).
-import(gen_tcp).
-import(erlang).
-import(ssl).

%% Pseudo-transport layer (interfaces directly with TCP).

connect(Conn, Host, Port, Opts) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, Opts),
    loop(Conn, Sock, fun gen_tcp:send/2).

send(Sock, Msg) ->
    Sock ! {bridge, self(), Msg}.

receive_data(Conn, Len, Data) ->
    .io:format("Got ~p~n", [Data]),
    Conn ! {tcp, Data}.

loop(Conn, S, Send) ->
    receive
        {bridge, Conn, ssl} ->
            .io:format("Upgrading to SSL!~n", []),
            {ok, SslSock} = ssl:connect(S, []),
            loop(Conn, SslSock, fun ssl:send/2);
        {bridge, Conn, Data} ->
	    .io:format("Sending ~p~n~n", [Data]),
            Len = byte_size(Data),
            Send(S, <<Len:32, Data/binary>>),
            loop(Conn, S, Send);
        {tcp, S, <<Len:32, Data/binary>>} ->
            receive_data(Conn, Len, Data),
	    loop(Conn, S, Send);
        {ssl, S, <<Len:32, Data/binary>>} ->
            receive_data(Conn, Len, Data),
	    loop(Conn, S, Send);
        {tcp_closed, S} ->
            .io:format("Closed! ~n", []),
            Conn ! disconnect,
            exit(normal);
	{sslsocket, new_ssl, NewSock} ->
	    loop(Conn, NewSock, Send);
        _Something ->
            .io:format("Unknown: ~p~n", [_Something]),
            .io:format("~p~n", [S]),
            loop(Conn, S, Send)
    end.
%% {ok, Pid} = bridge:start_link([{api_key, "951da7fb819d0ef3"}]).
