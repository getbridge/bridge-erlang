-module(bridge.tcp).
-export([send/2, connect/4]).
-import(gen_tcp).

%% Pseudo-transport layer (interfaces directly with TCP).

connect(Conn, Host, Port, Opts) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, Opts),
    loop(Conn, Sock).

send(Sock, Msg) ->
    Sock ! {bridge, self(), Msg}.

loop(Conn, S) ->
    receive
	{tcp, S, Data} ->
	    .io:format("Gots ~p~n", [Data]),
	    Conn ! {tcp, Data};
	{bridge, Conn, Data} ->
	    .io:format("Sending out ~p~n", [Data]),
	    gen_tcp:send(S, Data),
	    loop(Conn, S);
	{tcp_closed, S} ->
	    .io:format("Closed! ~n", []),
	    Conn ! disconnect,
	    exit(normal);
	_Something ->
	    .io:format("Unknown: ~p~n", [_Something]),
	    loop(Conn, S)
    end.
%% {ok, Pid} = bridge:start_link([{api_key, "aiflidndbjlgcikd"}, {host, "localhost"}, {port, 8090}]).
