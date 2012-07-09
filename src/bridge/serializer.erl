-module(bridge.serializer).
-behaviour(gen_server).

%% Presentation layer.

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).

-record(state,
	{ connection = undefined,
	  mappings   = dict:new()
	}).

init(_Args = {Opts, Parent}) ->
    {ok, dict:new(), bridge = Parent,
     connection = bridge.connection:start_link({Opts, self()})}.

handle_call(_Args, _From, State) ->
    {noreply, State}.

decode(Data, State) when is_list(Data) ->
    {Head, NewState} = decode(hd(Data), State),
    [Head | decode(tl(Data), NewState)];
decode(Data, State) ->
    {Data, State}.

encode(Data, State) when is_list(Data) ->
    {Head, NewState} = encode(hd(Data), State),
    [Head | encode(tl(Data), NewState)];
encode(Data, State) ->
    {Data, State}.

handle_cast({encode, Op, Args}, State = #state{connection = Conn}) ->
    {Encoded, NewState} = encode([{command, Op}, {data, Args}], State),
    bridge.connection:cast(Conn, jiffy:encode(Encoded)),
    {noreply, NewState};
handle_cast({decode, Op, Args}, State = #state{connection = Conn}) ->
    {Encoded, NewState} = decode([{command, Op}, {data, Args}], State),
    bridge.connection:cast(Conn, jiffy:decode(Encoded)),
    {noreply, NewState}.

handle_info(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
