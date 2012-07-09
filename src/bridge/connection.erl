-module(bridge.connection).
-behaviour(gen_server).

%% Session layer.

-export([start_link/1]).

-export([handle_call/3, handle_cast/2, handle_info/2,
         init/1]).

-export([code_change/3, terminate/2]).

-record(state,
        { socket	= undefined,
          serializer	= undefined,
          queue		= []      % calls to be flushed upon connection.
        }).

get_value(Key, PList) ->
    proplists:get_value(Key, PList).

start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

init({Opts, Serializer}) ->
    inets:start(),
    case get_value(secure, Opts) of
        true ->
            Options = [{redirector, get_value(secure_redirector, Opts)} | Opts];
        _ -> Options = Opts
    end,
    case dispatch(Options) of
        {ok, Sock} ->
            {ok, #state{socket = Sock, serializer = Serializer}};
        Msg -> {error, {redirector, Msg}}
    end.

%% Assuming Options is a proplist, still: will simply crash otherwise.
dispatch(Opts) ->
    case {get_value(host, Opts), get_value(port, Opts)} of
        {undefined, _} -> redirector(Opts);
        {_, undefined} -> redirector(Opts);
        {Host, Port} -> connect(Host, Port)
    end.

redirector(Opts) ->
    RedirUrl = get_value(redirector, Opts),
    ApiKey = get_value(api_key, Opts),
    redirector_response(httpc:request(RedirUrl ++ "/redirect/" ++ ApiKey)).

redirector_response({ok, {{_Vsn, 200, _Reason}, _Hd, Body}}) ->
    case get_value("data", bridge.serializer:parse_json(Body)) of
        undefined ->
            {error, Body};
        Data ->
            connect(get_value("bridge_host", Data),
                    get_value("bridge_port", Data))
    end;
redirector_response(_Res) -> {error, _Res}.

connect(Host, Port) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary]),
    Sock.

handle_cast(Data, State = #state{socket=TcpSock}) ->
    gen_tcp:send(TcpSock, Data),
    {ok, State}.

handle_call(_Request, _From, _State) ->
    ok.

handle_info({tcp, _Socket, Str}, State) ->
    {noreply, process_message(Str, State)};
handle_info(_Request, _State) ->
    {error, _Request}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

process_message(Msg, State = #state{serializer = Serializer}) ->
    gen_server:cast(Serializer, {decode, Msg}),
    State.
