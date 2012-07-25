-module(bridge.connection).
-behaviour(gen_server).

%% Session layer.

-export([start_link/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).

-import(erlang).
-import(ssl).

-import(inets).
-import(httpc).

-import(gen_server).

-import(proplists).
-import(binary).
-import(io_lib).
-import(lists).

-record(state,
        { socket        = undefined,
          serializer    = undefined,
          client_id     = undefined,
          queue         = []      % calls to be flushed upon connection.
        }).

-include("bridge_types.hrl").
-include("inet_types.hrl").

-spec get_val(json_key(), json_obj()) -> json();
             (json_key(), proplist(json_key(), _X :: term())) -> _X.
get_val(Key, {PList}) ->
    proplists:get_value(Key, PList);
get_val(Key, PList) ->
    proplists:get_value(Key, PList).

-spec start_link(options()) -> {ok, pid()}.
start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

-spec init({options(), pid()}) -> {ok, {#state{}, options()}}.
init({Opts, Serializer}) ->
    inets:start(),
    case get_val(secure, Opts) of
        true ->
            ssl:start(),
            Options = [{redirector, get_val(secure_redirector, Opts)} | Opts];
        _ ->
            Options = Opts
    end,
    {ok, {#state{serializer=Serializer}, Options}}.

%% Assuming Options is a proplist, still: will simply crash otherwise.
-spec dispatch(options()) -> {ok, pid()} | {error, _Reason :: term()}.
dispatch(Opts) ->
    {Host, Port} = {get_val(host, Opts), get_val(port, Opts)},
    case is_list(Host) andalso io_lib:char_list(Host)
        andalso is_integer(Port) andalso 0 < Port andalso Port < 65536 of
        true ->
            connect(Host, Port, get_val(secure, Opts));
        false ->
            redirector(Opts)
    end.

-spec redirector(options()) ->
                        {ok, pid()} | {error, _Reason :: term()}.
redirector(Opts) ->
    RedirUrl = get_val(redirector, Opts),
    ApiKey = atom_to_binary(get_val(api_key, Opts), utf8),
    Target = RedirUrl ++ "/redirect/" ++ [ApiKey],
    redirector_response(httpc:request(get, {Target, []}, [],
                                      [{body_format, binary}]),
                        Opts).

-spec redirector_response(httpc_result(), options()) ->
                                 {ok, pid()} | {error, term()}.
redirector_response({ok, {{_Vsn, 200, _Reason}, _Hd, Body}}, Opts) ->
    Json = bridge.serializer:parse_json(Body),
    case get_val(<<"data">>, Json) of
        undefined ->
            {error, Body};
        Data ->
            connect(binary_to_list(get_val(<<"bridge_host">>, Data)),
                    parse_int(get_val(<<"bridge_port">>, Data)),
                    get_val(secure, Opts) =:= true)
    end;
redirector_response(_Res, _Opts) -> {error, _Res}.

-spec parse_int(any()) -> integer().
parse_int(Term) ->
    if is_integer(Term) -> Term;
       is_list(Term)    -> list_to_integer(Term);
       is_binary(Term)  -> parse_int(binary_to_list(Term));
       true             -> 0
    end.

-spec connect(address(), port_number(), boolean()) -> {ok, pid()}.
connect(Host, Port, Secure) ->
    _Sock = erlang:spawn_link(bridge.tcp,
                              connect,
                              [self(),
                               Secure,
                               Host,
                               Port,
                               [binary, {active, true}]]),
    {ok, _Sock}.

-spec handle_cast({}, {#state{}, options()}) ->
                         {noreply, #state{}} | {stop, any(), #state{}}.
handle_cast({connect, Data}, {State, Options}) ->
    case dispatch(Options) of
        {ok, Sock} ->
            bridge.tcp:send(Sock, Data),
            {noreply, State#state{socket = Sock}};
        Msg -> {stop, {redirector, Msg}, State}
    end;
handle_cast(Data, State = #state{socket = Sock}) ->
    Sock ! {bridge, self(), Data},
    {noreply, State}.

handle_call(_Request, _From, _State) ->
    {noreply, _State}.

handle_info({tcp, Str}, State = #state{client_id = undefined}) ->
    NewState = extract_session(Str, State),
    FlushedState = flush_queue(NewState),
    {noreply, FlushedState};
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

-spec flush_queue(#state{}) -> #state{}.
flush_queue(_State = #state{queue = Q, socket = Sock}) ->
    lists:foreach(fun(X) -> bridge.tcp:send(Sock, X) end, Q),
    _State#state{queue = []}.

-spec process_message(binary(), #state{}) -> #state{}.
process_message(Msg, State = #state{serializer = Serializer}) ->
    gen_server:cast(Serializer, {decode, Msg}),
    State.

-spec extract_session(binary(), #state{}) -> #state{}.
extract_session(Str, State = #state{serializer = S}) ->
    [Id, Secret] = [binary_to_list(E) || E <- binary:split(Str, <<"|">>)],
    gen_server:cast(S, {connect_response, {Id, Secret}}),
    State#state{client_id = Id}.
