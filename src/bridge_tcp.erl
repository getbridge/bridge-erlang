-module(bridge_tcp).
-export([send/2, connect/6, receive_data/3]).

-export_type([hostname/0]).

-type hostname() :: inet:ip_address() | inet:hostname().

-record(state, {queue = [], reconnect, module, conn, host, port, opts}).

-spec connect(pid(), boolean(), boolean(), hostname(), inet:port_number(),
	      bridge:proplist(atom(), any())) -> no_return().

connect(Conn, Reconnect, Secure, Host, Port, Opts) ->
    if Secure ->
            Mod = ssl;
       true ->
            Mod = gen_tcp
    end,
    {ok, Sock} = Mod:connect(Host, Port, Opts),
    loop(Conn, Sock, fun Mod:send/2,
	 #state{reconnect = Reconnect, module = Mod, conn = Conn,
		host = Host, port = Port, opts = Opts}).

-spec send(pid(), binary()) -> {bridge, pid(), binary()}.
send(Sock, Msg) ->
    Sock ! {bridge, self(), Msg}.

-spec receive_data(pid(), [binary()], binary()) -> no_return().
receive_data(Conn, Buf, Data) ->
    Bin = list_to_binary([Buf, Data]),
    if byte_size(Bin) > 4 ->
            <<Len:32, Msg/binary>> = Bin,
            if Len >= byte_size(Msg) ->
		    <<First:Len/binary, Rest/binary>> = Msg,
                    Conn ! {tcp, First},
                    [Rest];
               true ->
                    [Msg]
            end;
       true ->
            [Bin]
    end.

-spec loop(pid(), inet:socket(), function(), #state{}) -> no_return().
loop(Conn, S, Send, State = #state{queue = Buf}) ->
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
            Conn ! {disconnect, tcp_closed},
            reconnect(State);
        {ssl_closed, S} ->
            Conn ! {disconnect, ssl_closed},
            reconnect(State);
        _Something ->
            .io:format("Unknown: ~p~n", [_Something]),
            .io:format("~p~n", [S]),
            loop(Conn, S, Send, Buf)
    end.

-spec reconnect(#state{}) -> no_return() | error.
reconnect(State = #state{host = Host, port = Port, conn = Conn,
			 module = Mod, opts = Opts}) ->
    {ok, Sock} = Mod:connect(Host, Port, Opts),
    loop(Conn, Sock, fun Mod:send/2, State#state{queue = []}).
