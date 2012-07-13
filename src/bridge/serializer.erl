-module(bridge.serializer).
-behaviour(gen_server).

%% Presentation layer.

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).
-export([start_link/1]).

-export([parse_json/1, find/2, store/2]).

-import(gen_server).
-import(erlang).
-import(random).
-import(lists).

-import(jiffy).
-import(dict).

-record(state,
        { connection = undefined,
          encode_map = dict:new(),
          decode_map = dict:new(),
	  client_id  = undefined,
          bridge     = undefined
        }).

find([_Type, _Own, _Svc, Key], Map) ->
    dict:find(Key, Map).

store(Key, State = #state{encode_map = Enc,
			  decode_map = Dec,
			  client_id  = Id  }) ->
    Str = list_to_binary([random:uniform(26)+96 || _X <- lists:seq(1,16)]),
    Val = [client, Id] ++ 
	if is_function(Key) ->
		[callback];
	   true ->
		[]
	end ++ [Str],
    {{[{ref, Val}]}, State#state{encode_map = dict:store(Key, Val, Enc),
		      decode_map = dict:store(Val, Key, Dec)}}.

start_link(Opts) ->
    random:seed(erlang:now()),
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

init(_Args = {Opts, Parent}) ->
    {ok, Conn} = bridge.connection:start_link(Opts),
    {ok, #state{bridge     = Parent,
                connection = Conn}}.

handle_call(_Args, _From, State) ->
    {noreply, State}.

decode([], State) ->
    {[], State};
decode([Head | Tail], State) ->
    {NewHead, NewState} = decode(Head, State),
    {NewTail, FinalState} = decode(Tail, NewState),
    {[NewHead | NewTail], FinalState};
decode({ref, Term}, State = #state{decode_map = Map}) when is_list(Term) ->
    case find(Term, Map) of
        error ->
            {undefined, State};
        {ok, Value} ->
            {Value, State}
    end;
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
encode({[{Key, V} | Tail]}, State) ->
    {NewV, NewState} = encode(V, State),
    {{NewTail}, FinalState} = encode({Tail}, NewState),
    {{[{Key, NewV} | NewTail]}, FinalState};
encode(Data, State) when is_function(Data) orelse is_pid(Data) ->
    store(Data, State);
encode(Data, State) ->
    {Data, State}.


handle_cast({connect, Data}, State = #state{connection = Conn}) ->
    gen_server:cast(Conn, {connect, jiffy:encode(Data)}),
    {noreply, State};
handle_cast(Msg = {connect_response, {_Id, _Secret}},
            State = #state{bridge = Bridge}) ->
    gen_server:cast(Bridge, Msg),
    {noreply, State#state{client_id = list_to_binary(_Id)}};
handle_cast({encode, {Op, Args}}, State = #state{connection = Conn}) ->
    {Encoded, NewState} = encode({[{command, Op}, {data, Args}]}, State),
    .io:format("ENCODED: ~p~n", [Encoded]),
    gen_server:cast(Conn, jiffy:encode(Encoded)),
    {noreply, NewState};
handle_cast({decode, Data}, State = #state{connection = Conn}) ->
    {Decoded, State} = decode(jiffy:decode(Data), State),
    gen_server:cast(Conn, Decoded),
    {noreply, State}.

handle_info({Conn, _Info}, State = #state{connection = Conn,
                                          bridge = Bridge}) ->
    Bridge ! {self(), _Info},
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

parse_json(Binary) ->
    jiffy:decode(Binary).

