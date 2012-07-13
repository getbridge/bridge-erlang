-module(bridge.connection).
-behaviour(gen_server).

%% Session layer.

-export([start_link/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).

-import(erlang).
-import(httpc).
-import(inets).
-import(ssl).

-import(proplists).
-import(gen_server).
-import(binary).
-import(lists).

-record(state,
        { socket	= undefined,
          serializer	= undefined,
	  client_id     = undefined,
          queue		= []      % calls to be flushed upon connection.
        }).

get_val(Key, {PList}) ->
    proplists:get_value(Key, PList);
get_val(Key, PList) ->
    proplists:get_value(Key, PList).

start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

init({Opts, Serializer}) ->
    inets:start(),
    case get_val(secure, Opts) of
        true ->
	    ssl:start(),
            Options = [{redirector, get_val(secure_redirector, Opts)} | Opts];
        _ -> Options = Opts
    end,
    {ok, {#state{serializer=Serializer}, Options}}.

%% Assuming Options is a proplist, still: will simply crash otherwise.
dispatch(Opts) ->
    case {get_val(host, Opts), get_val(port, Opts)} of
        {undefined, _} -> redirector(Opts);
        {_, undefined} -> redirector(Opts);
        {Host, Port}   -> connect(Host, Port)
    end.

redirector(Opts) ->
    RedirUrl = get_val(redirector, Opts),
    ApiKey = get_val(api_key, Opts),
    Target = RedirUrl ++ "/redirect/" ++ [ApiKey],
    redirector_response(httpc:request(get, {Target, []}, [],
				      [{body_format, binary}])).

redirector_response({ok, {{_Vsn, 200, _Reason}, _Hd, Body}}) ->
    Json = bridge.serializer:parse_json(Body),
    case get_val(<<"data">>, Json) of
        undefined ->
            {error, Body};
        Data ->
            connect(binary_to_list(get_val(<<"bridge_host">>, Data)),
                    list_to_integer( binary_to_list(get_val(<<"bridge_port">>,
							      Data))))
    end;
redirector_response(_Res) -> {error, _Res}.

connect(Host, Port) ->
    _Sock = erlang:spawn_link(bridge.tcp,
			      connect,
			      [self(),
			       Host,
			       Port,
			       [binary, {active, true}]]),
    {ok, _Sock}.

handle_cast({connect, Data}, {State, Options}) ->
    case dispatch(Options) of
        {ok, Sock} ->
	    case get_val(secure, Options) of
		true -> Sock ! {bridge, self(), ssl};
		_    -> ok
	    end,
	    .io:format("DATA: ~p~n", [Data]),
	    bridge.tcp:send(Sock, Data),
            {noreply, State#state{socket = Sock}};
        Msg -> {error, {redirector, Msg}}
    end;
handle_cast(Data, State = #state{socket = Sock}) ->
    Sock ! {bridge, self(), Data},
    {noreply, State}.

handle_call(_Request, _From, _State) ->
    {noreply, _State}.

handle_info({tcp, Str}, State = #state{client_id = undefined}) ->
    NewState = extract_session(Str, State),
    flush_queue(NewState),
    {noreply, NewState};
handle_info({tcp, Str}, State) ->
    {noreply, process_message(Str, State)};
handle_info(tcp_closed, State = #state{serializer = S}) ->
    S ! {self(), disconnect},
    {noreply, State};
handle_info(_Request, _State) ->
    {error, _Request}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

flush_queue(_State = #state{queue = Q, socket = Sock}) ->
    lists:foreach(fun(X) -> bridge.tcp:send(Sock, X) end, Q).

process_message(Msg, State = #state{serializer = Serializer}) ->
    gen_server:cast(Serializer, {decode, Msg}),
    State.

extract_session(Str, State = #state{serializer = S}) ->
    [Id, Secret] = [binary_to_list(E) || E <- binary:split(Str, <<"|">>)],
    gen_server:cast(S, {connect_response, {Id, Secret}}),
    State#state{client_id = Id}.
