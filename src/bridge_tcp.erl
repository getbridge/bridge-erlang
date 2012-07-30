-module(bridge_tcp).
-export([send/2, connect/6, receive_data/3]).

-export_type([hostname/0]).

-type hostname() :: inet:ip_address() | inet:hostname().

-record(state, {queue = [], reconnect, module, conn, host, port, opts}).

-spec connect(pid(), boolean(), boolean(), hostname(), inet:port_number(),
	      bridge:proplist(atom(), any())) -> no_return().

connect(Conn, Reconnect, Secure, Host, Port, Opts) when is_pid(Conn) andalso
							is_boolean(Reconnect) andalso
							is_boolean(Secure) ->
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
loop(Conn, Sock, Send, S = #state{queue = Q}) ->
    receive
        {bridge, Conn, Data} ->
            Len = byte_size(Data),
            Send(Sock, <<Len:32, Data/binary>>),
            loop(Conn, Sock, Send, S);
        {tcp, Sock, Data} ->
            loop(Conn, Sock, Send, receive_data(Conn, Q, Data));
        {ssl, Sock, Data} ->
            loop(Conn, Sock, Send, S#state{queue = receive_data(Conn, Q, Data)});
        {tcp_closed, Sock} ->
            Conn ! {disconnect, tcp_closed},
            reconnect(S);
        {ssl_closed, Sock} ->
            Conn ! {disconnect, ssl_closed},
            reconnect(S);
        _Something ->
            .io:format("Unknown: ~p~n", [_Something]),
            .io:format("~p~n", [Sock]),
            loop(Conn, Sock, Send, S)
    end.

-spec reconnect(#state{}) -> no_return() | error.
reconnect(State = #state{host = Host, port = Port, conn = Conn,
			 module = Mod, opts = Opts}) ->
    {ok, Sock} = Mod:connect(Host, Port, Opts),
    loop(Conn, Sock, fun Mod:send/2, State#state{queue = []}).
