-module(bridge_tcp).
-export([send/2, connect/5, receive_data/3]).
-import(gen_tcp).
-import(erlang).
-import(binary).
-import(ssl).

-export_type([hostname/0, port_number/0]).

-type hostname() :: atom() | string().
-type port_number() :: 0..65535.
-type socket() :: pid() | {sslsocket, new_ssl, pid()}.

-spec connect(pid(), boolean(), hostname(),
	      port_number(), bridge:proplist(atom(), any())) -> no_return().
connect(Conn, Secure, Host, Port, Opts) ->
    if Secure ->
            Mod = ssl;
       true ->
            Mod = gen_tcp
    end,
    {ok, Sock} = Mod:connect(Host, Port, Opts),
    loop(Conn, Sock, fun Mod:send/2, []).

-spec send(pid(), binary()) -> {bridge, pid(), binary()}.
send(Sock, Msg) ->
    Sock ! {bridge, self(), Msg}.

-spec receive_data(pid(), [binary()], binary()) -> no_return().
receive_data(Conn, Buf, Data) ->
    Bin = list_to_binary([Buf, Data]),
    if byte_size(Bin) > 4 ->
            <<Len:32, Msg/binary>> = Bin,
            if Len >= erlang:byte_size(Msg) ->
		    <<First:Len/binary, Rest/binary>> = Msg,
                    Conn ! {tcp, First},
                    [Rest];
               true ->
                    [Msg]
            end;
       true ->
            [Bin]
    end.

-spec loop(pid(), socket(), function(), [binary()]) -> no_return().
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
