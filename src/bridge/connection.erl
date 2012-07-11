-module(bridge.connection).
-behaviour(gen_server).

%% Session layer.

-export([start_link/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).

-import(proplists).
-import(gen_server).
-import(erlang).
-import(httpc).
-import(inets).
-import(ssl).

-record(state,
        { socket	= undefined,
          serializer	= undefined,
	  client_id     = undefined,
          queue		= []      % calls to be flushed upon connection.
        }).

get_value(Key, {PList}) ->
    proplists:get_value(Key, PList);
get_value(Key, PList) ->
    proplists:get_value(Key, PList).

start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

init({Opts, Serializer}) ->
    inets:start(),
    case get_value(secure, Opts) of
        true ->
	    ssl:start(),
            Options = [{redirector, get_value(secure_redirector, Opts)} | Opts];
        _ -> Options = Opts
    end,
    {ok, {#state{serializer=Serializer}, Options}}.

%% Assuming Options is a proplist, still: will simply crash otherwise.
dispatch(Opts) ->
    case {get_value(host, Opts), get_value(port, Opts)} of
        {undefined, _} -> redirector(Opts);
        {_, undefined} -> redirector(Opts);
        {Host, Port}   -> connect(Host, Port)
    end.

redirector(Opts) ->
    RedirUrl = get_value(redirector, Opts),
    ApiKey = get_value(api_key, Opts),
    Target = RedirUrl ++ "/redirect/" ++ ApiKey,
    redirector_response(httpc:request(get, {Target, []}, [],
				      [{body_format, binary}])).

redirector_response({ok, {{_Vsn, 200, _Reason}, _Hd, Body}}) ->
    Json = bridge.serializer:parse_json(Body),
    case get_value(<<"data">>, Json) of
        undefined ->
            {error, Body};
        Data ->
            connect(binary_to_list(get_value(<<"bridge_host">>, Data)),
                    list_to_integer( binary_to_list(get_value(<<"bridge_port">>,
							      Data))))
    end;
redirector_response(_Res) -> {error, _Res}.

connect(Host, Port) ->
    _Sock = erlang:spawn_link(bridge.tcp,
			      connect,
			      [self(),
			       Host,
			       Port,
			       [binary]]),
    {ok, _Sock}.

handle_cast(connect, {State, Options}) ->
    ApiKey = list_to_binary([get_value(api_key, Options)]),
    case dispatch(Options) of
        {ok, Sock} ->
	    case get_value(secure, Options) of
		true -> Sock ! {bridge, self(), ssl};
		_    -> ok
	    end,
	    bridge.tcp:send(Sock,
			    <<"{\"command\":\"CONNECT\",\"data\":",
			    "{\"session\":[null,null],\"api_key\":",
			    "\"", ApiKey/binary, "\"}}">>),
            {noreply, State#state{socket = Sock}};
        Msg -> {error, {redirector, Msg}}
    end;
handle_cast(Data, {State = #state{queue = Q}, _Opts}) ->
    {noreply, {State#state{queue = [Data] ++ Q}, _Opts}}.

handle_call(_Request, _From, _State) ->
    {noreply, _State}.

handle_info({tcp, _Socket, Str}, State = #state{client_id = undefined}) ->
    .io:format("Gots info: ~p.~n", [Str]),
    NewState = extract_session(Str, State),
    flush_queue(NewState),
    {noreply, NewState};
handle_info({tcp, _Socket, Str}, State) ->
    {noreply, process_message(Str, State)};
handle_info(tcp_closed, State) ->
    .io:format("socket closed!"),
    {noreply, State};
handle_info(_Request, _State) ->
    {error, _Request}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

flush_queue(State = #state{queue = Q, socket = Sock}) ->
    lists:foreach(fun(X) -> bridge.tcp:send(Sock, X) end, Q).

process_message(Msg, State = #state{serializer = Serializer}) ->
    .io:format("Msg: ~p~n", [Msg]),
    gen_server:cast(Serializer, {decode, Msg}),
    State.

extract_session(Str = <<Id, "|", Secret>>, State = #state{serializer = S}) ->
    .io:format("Id: ~p, Secret: ~p~n", [Id, Secret]),
    gen_server:cast(S, {connect_response, {Id, Secret}}),
    State#state{client_id = Id}.
