-module(bridge_connection).
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
          encoder    = undefined,
          client_id     = undefined,
          queue         = []      % calls to be flushed upon connection.
        }).

-type address() :: ip_address() | bridge_tcp:hostname().
-type ip_address() :: ip4_address() | ip6_address().
-type ip4_address() :: { byte(), byte(), byte(), byte() }.

-type ip6_address() :: { 0..65535, 0..65535, 0..65535, 0..65535, 
                         0..65535, 0..65535, 0..65535, 0..65535  }.

-type httpc_result() :: {status_line(), headers(), httpc_body()}
                      | {status_code(), httpc_body()}.

-type httpc_body() :: string() | binary().
-type status_line()    :: {http_version(), status_code(), reason_phrase()}.
-type http_version()   :: string().
-type status_code()    :: pos_integer().
-type reason_phrase()  :: string().
-type headers()        :: [header()].
-type header()         :: {string(), string()}.


-spec get_val(bridge:json_key(), bridge:json_obj()) -> bridge:json();
             (bridge:json_key(), bridge:proplist(bridge:json_key(), any())) ->
                     any().
get_val(Key, {PList}) ->
    proplists:get_value(Key, PList);
get_val(Key, PList) ->
    proplists:get_value(Key, PList).

-spec start_link(bridge:options()) -> {ok, pid()}.
start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

-spec init({bridge:options(), pid()}) -> {ok, {#state{}, bridge:options()}}.
init({Opts, Encoder}) ->
    inets:start(),
    case get_val(secure, Opts) of
        true ->
            ssl:start(),
            Options = [{redirector, get_val(secure_redirector, Opts)} | Opts];
        _ ->
            Options = Opts
    end,
    {ok, {#state{encoder=Encoder}, Options}}.

%% Assuming Options is a proplist, still: will simply crash otherwise.
-spec dispatch(bridge:options()) -> {ok, pid()} | {error, _Reason :: term()}.
dispatch(Opts) ->
    {Host, Port} = {get_val(host, Opts), get_val(port, Opts)},
    case is_list(Host) andalso io_lib:char_list(Host) andalso
	is_integer(Port) andalso 0 < Port andalso Port < 65536 of
        true ->
            connect(Host, Port, get_val(secure, Opts));
        false ->
            redirector(Opts)
    end.

-spec redirector(bridge:options()) -> {ok, pid()} | {error, _Reason :: term()}.
redirector(Opts) ->
    RedirUrl = get_val(redirector, Opts),
    ApiKey = atom_to_binary(get_val(api_key, Opts), utf8),
    Target = RedirUrl ++ "/redirect/" ++ [ApiKey],
    redirector_response(httpc:request(get, {Target, []}, [],
                                      [{body_format, binary}]), Opts).

-spec redirector_response(httpc_result(), bridge:options()) ->
                                 {ok, pid()} | {error, term()}.
redirector_response({ok, {{_Vsn, 200, _Reason}, _Hd, Body}}, Opts) ->
    Json = bridge_encoder:parse_json(Body),
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

-spec connect(address(), bridge_tcp:port_number(), boolean()) -> {ok, pid()}.
connect(Host, Port, Secure) ->
    _Sock = erlang:spawn_link(bridge_tcp, connect,
                              [self(), Secure, Host, Port,
                               [binary, {active, true}]]),
    {ok, _Sock}.

-spec handle_cast({}, {#state{}, bridge:options()}) ->
                         {noreply, #state{}} | {stop, any(), #state{}}.
handle_cast({connect, Data}, {State, Options}) ->
    case dispatch(Options) of
        {ok, Sock} ->
            bridge_tcp:send(Sock, Data),
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
handle_info(tcp_closed, State = #state{encoder = S}) ->
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
    lists:foreach(fun(X) -> bridge_tcp:send(Sock, X) end, Q),
    _State#state{queue = []}.

-spec process_message(binary(), #state{}) -> #state{}.
process_message(Msg, State = #state{encoder = Encoder}) ->
    gen_server:cast(Encoder, {decode, Msg}),
    State.

-spec extract_session(binary(), #state{}) -> #state{}.
extract_session(Str, State = #state{encoder = S}) ->
    [Id, Secret] = [binary_to_list(E) || E <- binary:split(Str, <<"|">>)],
    gen_server:cast(S, {connect_response, {Id, Secret}}),
    State#state{client_id = Id}.
