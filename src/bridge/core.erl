-module(bridge.core).
-behaviour(gen_server).

%% TODO: Enforce style consistency.

-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).
-export([call/2]).

-import(gen_server).
-import(proplists).
-import(random).
-import(lists).
-import(dict).

-record(state,
        { opts          = [],
          buffer        = [],
          context       = undefined,
          event_handler = undefined,
          encode_map    = dict:new(),
          decode_map    = dict:new(),
          connected     = false,
          client_id     = null,
          secret        = null,
          api_key,
          serializer
        }).

start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

init(Options) ->
    {A1, A2, A3} = erlang:now(),
    random:seed(A1, A2, A3),
    Opts = Options ++
        [{log, 2},
         {redirector, "http://redirector.getbridge.com"},
         {secure_redirector, "https://redirector.getbridge.com"},
         {secure, true}],
    Key = proplists:get_value(api_key, Opts),
    {ok, S} = bridge.serializer:start_link(Opts),
    connect(#state{opts = Opts, serializer = S, api_key = Key}).

call(Pid, {Method, Args}) when is_atom(Method) ->
    gen_server:call(Pid, {Method, Args}).

handle_call(context, _From, State) ->
    {reply, context(State), State};
handle_call(_Stuff, _From, State) ->
    {reply, ok, State}.

connect(State = #state{api_key=Key, client_id = Id,
                       secret=Secret, serializer=S}) ->
    gen_server:cast(S, {connect, {[{command, 'CONNECT'},
                                   {data, {[{api_key, Key},
                                            {session, [Id, Secret]}]}
                                   }]}
                       }),
    {ok, State}.

handle_cast({outbound, {Op, Data}}, _State) ->
    NewState = send_command(Op, Data, _State),
    {noreply, NewState};
handle_cast({add_handler, Mod}, _State) ->
    {ok, NewState} = add_handler(Mod, _State),
    {noreply, NewState};
handle_cast({_Data}, S) ->
    {Data, S} = decode(_Data, S),
    .io:format("CASTED data: ~p~n", [Data]),
    {{[{Dest, Method}]}, Args} = {proplists:get_value(<<"destination">>, Data),
		    proplists:get_value(<<"args">>, Data)},
    if is_function(Dest) ->
	    apply(Dest, Args);
       true ->
	    .io:format("dest: ~p, method: ~p~n", [Dest, Method]),
	    gen_server:cast(Dest, {Method, Args})
    end,
    {noreply, S};
handle_cast({connect_response, {Id, Secret}}, State = #state{buffer = Buf}) ->
    %% Flush queue.
    .io:format("Flushing ~p entries~n", [length(Buf)]),
    NewState = lists:foldr(fun(Elem, AccIn) -> Elem(AccIn) end,
			   State#state{ client_id = Id, secret = Secret,
					connected = true, buffer = []},
			   Buf),
    {noreply, NewState};
handle_cast(connect, State) ->
    connect(State),
    {noreply, State}.


handle_info(_Info, State = #state{event_handler = undefined}) ->
    {noreply, State};
handle_info(Info, State = #state{event_handler = E}) ->
    gen_event:notify(E, Info),
    {noreply, State}.


add_handler(E, State = #state{}) ->
    {ok, Ev} = gen_server:start(E),
    {ok, State#state{event_handler = Ev}}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send_command(Op, Args, _State = #state{serializer = S, connected = false,
                                       buffer = Buf}) ->
    case Op of
	'JOINWORKERPOOL' ->
	    State = bind_args(<<"named">>, Args, _State);
	'JOINCHANNEL' ->
	    State = bind_args(<<"channel">>, Args, _State);
	_ ->
	    State = _State
    end,
    State#state{
      buffer = [ fun(CurrState) ->
			 {Encoded, NewState} = encode(Args, CurrState),
			 gen_server:cast(S, {encode, {Op, Encoded}}),
			 NewState
		 end | Buf ]
     };
send_command(Op, Args, State = #state{serializer = S}) ->
    gen_server:cast(S, {encode, {Op, Args}}),
    State.

context(#state{context = Context}) ->
    Context.


bind_args(Type, {Args}, _State = #state{decode_map = Dec}) ->
    Name = atom_to_binary(proplists:get_value(name, Args), utf8),
    Handler = proplists:get_value(handler, Args),
    Id = case Type of
	     <<"named">> ->
		 Name;
	     <<"channel">> ->
		 <<"channel:", Name/binary>>
	 end,
    _State#state{decode_map = dict:store([Type, Id, Name], Handler, Dec)}.

find(Key, Map) ->
    dict:find(Key, Map).

store(Key, State = #state{encode_map = Enc,
			  decode_map = Dec,
			  client_id  = Id  }) ->
    Str = list_to_binary([random:uniform(26)+96 || _X <- lists:seq(1,16)]),
    Val = [client, list_to_binary(Id)] ++ 
	if is_function(Key) ->
		[callback];
	   true ->
		[]
	end ++ [Str],
    {{[{ref, Val}]}, State#state{encode_map = dict:store(Key, Val, Enc),
				 decode_map = dict:store(Val, Key, Dec)}}.


decode([], State) ->
    {[], State};
decode([Head | Tail], State) ->
    {NewHead, State} = decode(Head, State),
    {NewTail, State} = decode(Tail, State),
    {[NewHead | NewTail], State};
decode({<<"ref">>, T}, State = #state{decode_map = Map}) when is_list(T) ->
    case find(T, Map) of
        error ->
	    {Root, Method} = {lists:sublist(T, 3), lists:last(T)},
	    if Root == T ->
		    {{<<"ref">>, T}, State};
	       true ->
		    {Decoded, State} = decode({<<"ref">>, Root}, State),
		    if is_tuple(Decoded) ->
			    {<<"ref">>, Lst} = Decoded,
			    {{<<"ref">>, Lst ++ [Method]}, State};
		       true ->
			    {{Decoded, Method}, State}
		    end
	    end;
        {ok, Value} ->
            {Value, State};
	_Something ->
	    .io:format("Something unexpected found: ~p~n", [_Something])
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
