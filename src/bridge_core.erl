-module(bridge_core).
-behaviour(gen_server).

-define(Ref(X), {[{<<"ref">>, X}]}).
-define(Ref(Pid, X), {[{<<"ref">>, X}, {<<"operations">>, exports(Pid)}]}).

%% TODO: Enforce style consistency.

-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).

-export([reconnect/2]).

-define(DEFAULT_OPTIONS,
        [{log, 2},
         {redirector, "http://redirector.getbridge.com"},
         {secure_redirector, "https://redirector.getbridge.com"},
         {secure, true},
         {reconnect, true}]).

-record(state,
        { opts          = [],
          queue         = [],
          context       = undefined,
          event_handler = undefined,
          encode_map    = dict:new(),
          decode_map    = dict:new(),
          connected     = false,
          id            = null,
          secret        = null,
          encoder,
          key
        }).

exports(Pid) when is_function(Pid) ->
    [callback];
exports(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, exports).

-spec start_link(bridge:options()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Opts) -> gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

seed() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3).

-spec init(bridge:options()) -> {ok, #state{}}.
init(Options) ->
    seed(),
    Opts = Options ++ ?DEFAULT_OPTIONS,
    Key = proplists:get_value(api_key, Opts),
    {ok, E} = bridge_encoder:start_link(Opts),
    {ok, #state{opts = Opts, encoder = E, key = Key}}.

-spec handle_call(context,
                  {pid(), any()}, #state{}) -> {reply, binary(), #state{}}.
handle_call(context, _From, State) ->
    {reply, context(State), State}.

-spec connect(#state{}) -> #state{}.
connect(State = #state{key = Key, id = Id, secret = Secret, encoder = E}) ->
    Data = {[{api_key, Key}, {session, [Id, Secret]}]},
    gen_server:cast(E, {connect, {[{command, 'CONNECT'}, {data, Data}]}}),
    State.

handle_cast({outbound, {Op, Data}}, S) ->
    {noreply, send_command(Op, Data, S)};
handle_cast({add_handler, Mod}, S) ->
    {ok, NewState} = add_handler(Mod, S),
    {noreply, NewState};
handle_cast({_Data}, S) ->
    {Data, S} = decode(_Data, S),
    [Dst, Args, Src] = [proplists:get_value(atom_to_binary(X, utf8), Data)
                        || X <- [destination, args, source]],
    {Dest, Method} = unpack_dest(Dst),
    State = invoke(Dest, Method, Args, S),
    {noreply, State#state{context = Src}};
handle_cast({connect_response, {Id, S}}, State = #state{queue = Q}) ->
    {noreply,
     lists:foldr(
       fun(E, Acc) -> E(Acc) end,
       State#state{id = Id, secret = S, connected = true, queue = []}, Q
      )};
handle_cast(connect, State) ->
    {noreply, connect(State)}.

handle_info({E, Info = {disconnect, _}}, State = #state{event_handler = Ev,
                                                        encoder = E,
							opts = Opts}) ->
    if is_pid(Ev) ->
            gen_event:notify(Ev, Info);
       true ->
            ok
    end,
    R = proplists:get_value(reconnect, Opts),
    if R ->
	    timer:apply_after(100, ?MODULE, reconnect, [self(), 100]);
       true ->
	    ok
    end,
    {noreply, State#state{connected = false}};
handle_info(_Info, State = #state{event_handler = undefined}) ->
    {noreply, State};
handle_info({_From, Info}, State = #state{event_handler = E}) ->
    gen_event:notify(E, Info),
    {noreply, State}.

-spec add_handler(pid(), #state{}) -> {ok, #state{}}.
add_handler(E, State = #state{}) ->
    {ok, State#state{event_handler = E}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec update_state(bridge:json_key(), [bridge:json()], #state{}) -> #state{}.
update_state(Op, Args, State = #state{encoder = S}) ->
    {Encoded, NewState} = encode(Args, State),
    gen_server:cast(S, {encode, {Op, Encoded}}),
    if Op == 'JOINWORKERPOOL' ->
            bind_args(<<"named">>, Args, NewState);
       true ->
            NewState
    end.

send_command(Op, Args, State = #state{connected = false, queue = Q}) ->
    State#state{queue = [fun(S) -> update_state(Op, Args, S) end | Q]};

send_command(Op, Args, State = #state{encoder = S}) ->
    {Encoded, NewState} = encode(Args, State),
    gen_server:cast(S, {encode, {Op, Encoded}}),
    NewState.

context(#state{context = Context}) ->
    Context.

-spec invoke(bridge:service(), bridge:json_key(), [bridge:json()], #state{}) ->
                    #state{}.
invoke(Dest, Method, Args, S) when is_tuple(Dest) ->
    ?Ref(Dst) = Dest,
    case lists:nth(3, Dst) of
        <<"system">> ->
            syscall(Method, Args, S);
        _NoClue ->
            %% we don't have this method...?
            S
    end;
invoke(Dest, _Method, Args, S) when is_function(Dest) ->
    bridge:cast(self(), {Dest, _Method, Args}),
    S;
invoke(Dest, Method, Args, S) when is_pid(Dest) ->
    bridge:cast(self(), {Dest, to_atom(Method), Args}),
    S.

-spec to_atom(binary() | atom()) -> atom().
to_atom(Item) ->
    if is_binary(Item) ->
            binary_to_existing_atom(Item, utf8);
       is_atom(Item) ->
            Item;
       true ->
            undefined
    end.

-spec to_binary(binary() | atom() | [char()]) -> binary().
to_binary(Name) ->
    if is_atom(Name) ->
            atom_to_binary(Name, utf8);
       is_binary(Name) ->
            Name;
       is_list(Name) ->
            list_to_binary(lists:map(fun to_binary/1, Name));
       true ->
            <<"unknown_name">>
    end.

bind_args(Type, {Args}, S = #state{decode_map = Dec, id = Self}) ->
    Name = to_binary(proplists:get_value(name, Args)),
    Handler = proplists:get_value(handler, Args),
    Id = case Type of
             <<"named">> ->
                 Name;
             <<"channel">> ->
                 <<"channel:", Name/binary>>
         end,
    Map = dict:store([Type, Name, Id], Handler, Dec),
    S#state{decode_map = dict:store([<<"client">>, list_to_binary(Self), Id],
                                    Handler, Map)}.

find(Key, Map) ->
    dict:find(Key, Map).

store(Key, State = #state{encode_map = Enc,
                          decode_map = Dec,
                          id  = Id  }) ->
    Str = list_to_binary([random:uniform(26)+96 || _X <- lists:seq(1,16)]),
    Val = [<<"client">>, list_to_binary(Id), Str],
    {?Ref(Key, Val), State#state{encode_map = dict:store(Key, Val, Enc),
                                 decode_map = dict:store(Val, Key, Dec)}}.


decode([], State) ->
    {[], State};
decode([Head | Tail], State) ->
    {NewHead, State} = decode(Head, State),
    {NewTail, State} = decode(Tail, State),
    {[NewHead | NewTail], State};
decode(?Ref(T), State = #state{decode_map = Map}) when is_list(T) ->
    case find(T, Map) of
        {ok, Value} ->
            {Value, State};
        error ->
            {Root, Method} = {lists:sublist(T, 3), lists:last(T)},
            Svc = lists:last(Root),
            if Root == T ->
                    {?Ref(T), State};
               Svc == <<"system">> ->
                    {{?Ref(Root), Method}, State};
               true ->
                    {Decoded, State} = decode(?Ref(Root), State),
                    if is_tuple(Decoded) ->
                            ?Ref(Lst) = Decoded,
                            {?Ref(Lst ++ [Method]), State};
                       true ->
                            {{Decoded, Method}, State}
                    end
            end
    end;
decode({_Key, Term}, State) ->
    {Value, State} = decode(Term, State),
    {{_Key, Value}, State};
decode({Term}, State) ->
    {Value, State} = decode(Term, State),
    {{Value}, State};
decode(Data, State) ->
    {Data, State}.


encode([], State) ->
    {[], State};
encode([Head | Tail], State) ->
    {NewHead, NewState} = encode(Head, State),
    {NewTail, FinalState} = encode(Tail, NewState),
    {[NewHead | NewTail], FinalState};

encode({[]}, State) ->
    {{[]}, State};
encode({[{_Head, undefined} | Tail]}, State) ->
    encode({Tail}, State);
encode({[{Key, V} | Tail]}, State) ->
    {NewV, NewState} = encode(V, State),
    {{NewTail}, FinalState} = encode({Tail}, NewState),
    {{[{Key, NewV} | NewTail]}, FinalState};
encode(Data, State) when is_function(Data) orelse is_pid(Data) ->
    store(Data, State);
encode(Data, State) ->
    {Data, State}.

syscall(<<"hookChannelHandler">>, [Name, Handler], State) ->
    syscall(<<"hookChannelHandler">>, [Name, Handler, undefined], State);
syscall(<<"hookChannelHandler">>, [Name, Handler, Func], _S) ->
    State = bind_args(<<"channel">>, {[{name, Name}, {handler, Handler}]}, _S),
    if Func == undefined ->
            ok;
       true ->
            Path = [channel, Name, <<"channel:", Name/binary>>],
            bridge:cast(self(), {Func, callback, [?Ref(Path), Name]})
    end,
    State;
syscall(<<"getService">>, [Name, Func], _State = #state{decode_map = Map} ) ->
    {ok, Value} = find([<<"named">>, Name, Name], Map),
    bridge:cast(self(), { Func, callback, [Value, Name] }),
    _State;
syscall(<<"remoteError">>, [Msg], _State) ->
    self() ! {error, {remote_error, Msg}},
    _State.

-spec unpack_dest(bridge:remote_service()) ->
                         {bridge:service(), bridge:json_key()};
                 ({bridge:service(), bridge:json_key()}) ->
                         {bridge:service(), bridge:json_key()}.
unpack_dest(?Ref([Type, Id, Handler, Method])) ->
    {?Ref([Type, Id, Handler]), Method};
unpack_dest(D = {_Dest, _Method}) -> D.
-spec reconnect(pid(), integer()) -> ok.

reconnect(Pid, Timeout) when Timeout > 10000 ->
    Pid ! {Pid, {error, reconnect_timeout}};
reconnect(Pid, Timeout) ->
    Connected = gen_server:call(Pid, is_connected),
    if Connected ->
	    ok;
       true ->
	    bridge:connect(Pid),
	    timer:apply_after(Timeout * 2, ?MODULE, reconnect,
			      [self(), Timeout * 2])
    end.
