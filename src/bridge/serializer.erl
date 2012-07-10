-module(bridge.serializer).
-behaviour(gen_server).

%% Presentation layer.

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).
-export([start_link/1]).

-export([parse_json/1]).

-import(gen_server).
-import(jiffy).
-import(dict).

-record(state,
	{ connection = undefined,
	  mappings   = dict:new(),
	  bridge     = undefined
	}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

init(_Args = {Opts, Parent}) ->
    {ok, Conn} = bridge.connection:start_link(Opts),
    {ok, #state{mappings   = dict:new(),
		bridge     = Parent,
		connection = Conn}}.

handle_call(_Args, _From, State) ->
    {noreply, State}.

decode(Data, State) when is_list(Data) ->
    {Head, NewState} = decode(hd(Data), State),
    [Head | decode(tl(Data), NewState)];
decode(Data, State) ->
    {Data, State}.

encode([], State) ->
    {[], State};
encode([Head, Tail], State) ->
    {NewHead, NewState} = encode(Head, State),
    [NewHead | encode(Tail, NewState)];
encode(Data, State) ->
    {Data, State}.

handle_cast(connect, State = #state{connection = Conn}) ->
    gen_server:cast(Conn, connect),
    {noreply, State};
handle_cast(Msg = {connect_response, {Id, Secret}},
	    State = #state{connection = Conn}) ->
    gen_server:cast(Conn, Msg),
    {noreply, State};
handle_cast({encode, Op, Args}, State = #state{connection = Conn}) ->
    {Encoded, NewState} = {{[{command, Op}, {data, Args}]}, State},
    gen_server:cast(Conn, jiffy:encode(Encoded)),
    {noreply, NewState};
handle_cast({decode, Data}, State = #state{connection = Conn}) ->
    Decoded = jiffy:decode(Data),
    gen_server:cast(Conn, Decoded),
    {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

parse_json(Binary) ->
    jiffy:decode(Binary).

