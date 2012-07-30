-module(bridge_tcp).
-export([send/2, connect/5, receive_data/3]).

-export_type([hostname/0]).

-type hostname() :: inet:ip_address() | inet:hostname().

-record(state, {queue = [], conn}).

-spec connect(pid(), boolean(), hostname(), inet:port_number(),
              bridge:proplist(atom(), any())) -> no_return().

connect(Conn, Secure, Host, Port, Opts) when is_pid(Conn) andalso
                                             is_boolean(Secure) ->
    if Secure ->
            Mod = ssl;
       true ->
            Mod = gen_tcp
    end,
    {ok, Sock} = Mod:connect(Host, Port, Opts),
    loop(Sock, fun Mod:send/2, #state{conn = Conn}).

-spec send(pid(), binary()) -> {bridge, pid(), binary()}.
send(Sock, Msg) ->
    Sock ! {bridge, self(), Msg}.

-spec receive_data(pid(), [binary()], binary()) -> [binary()].
receive_data(Conn, Buf, Data) ->
    Bin = list_to_binary([Buf, Data]),
    if byte_size(Bin) > 4 ->
            <<Len:32, Msg/binary>> = Bin,
            if Len >= byte_size(Msg) ->
                    <<First:Len/binary, Rest/binary>> = Msg,
                    Conn ! {tcp, First},
                    [Rest];
               true ->
                    [Bin]
            end;
       true ->
            [Bin]
    end.

-spec loop(inet:socket(), function(), #state{}) -> no_return().
loop(Sock, Send, S = #state{queue = Q, conn = Conn}) ->
    receive
        {bridge, Conn, Data} ->
            Len = byte_size(Data),
            Send(Sock, <<Len:32, Data/binary>>),
            loop(Sock, Send, S);
        {tcp, Sock, Data} ->
            loop(Sock, Send, S#state{queue = receive_data(Conn, Q, Data)});
        {ssl, Sock, Data} ->
            loop(Sock, Send, S#state{queue = receive_data(Conn, Q, Data)});
        {tcp_closed, Sock} ->
            Conn ! {disconnect, tcp_closed},
            exit(normal);
        {ssl_closed, Sock} ->
            Conn ! {disconnect, ssl_closed},
            exit(normal);
        _Something ->
            Conn ! {unknown, _Something},
            loop(Sock, Send, S)
    end.
