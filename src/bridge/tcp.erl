-module(bridge.tcp).
-export([send/2, connect/4]).
-import(gen_tcp).
-import(erlang).
-import(ssl).

%% Pseudo-transport layer (interfaces directly with TCP).

connect(Conn, Host, Port, Opts) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, Opts),
    loop(Conn, Sock, fun gen_tcp:send/2).

send(Sock, Msg) ->
    Sock ! {bridge, self(), Msg}.

loop(Conn, S, Send) ->
    receive
	{bridge, Conn, ssl} ->
	    .io:format("Upgrading to SSL!~n", []),
	    {ok, SslSock} = ssl:connect(S, []),
	    loop(Conn, SslSock, fun ssl:send/2);
	{tcp, S, Data} ->
	    .io:format("Gots ~p~n", [Data]),
	    Conn ! {tcp, Data};
	{bridge, Conn, Data} ->
	    .io:format("Sending out ~p~n", [Data]),
	    Send(S, Data),
	    loop(Conn, S, Send);
	{tcp_closed, S} ->
	    .io:format("Closed! ~n", []),
	    Conn ! disconnect,
	    exit(normal);
	_Something ->
	    .io:format("Unknown: ~p~n", [_Something]),
	    loop(Conn, S, Send)
    end.
%% {ok, Pid} = bridge:start_link([{api_key, "951da7fb819d0ef3"}]).
