-module(bridge.serializer).
-behaviour(gen_server).

%% Presentation layer.

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).
-export([start_link/1]).

-export([parse_json/1]).

-import(gen_server).
-import(erlang).
-import(lists).

-import(jiffy).

-include("bridge_types.hrl").

-record(state,
        { connection = undefined,
          bridge     = undefined
        }).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

init(_Args = {Opts, Parent}) ->
    .io:format("Starting serializer.~n"),
    {ok, Conn} = bridge.connection:start_link(Opts),
    .io:format("Started connection.~n"),
    {ok, #state{bridge     = Parent,
                connection = Conn}}.

handle_call(_Args, _From, State) ->
    {noreply, State}.

handle_cast({connect, Data}, State = #state{connection = Conn}) ->
    gen_server:cast(Conn, {connect, jiffy:encode(Data)}),
    {noreply, State};
handle_cast(Msg = {connect_response, {_Id, _Secret}},
            State = #state{bridge = Bridge}) ->
    gen_server:cast(Bridge, Msg),
    {noreply, State};
handle_cast({encode, {Op, Args}}, State = #state{connection = Conn}) ->
    gen_server:cast(Conn, jiffy:encode({[{command, Op}, {data, Args}]})),
    {noreply, State};
handle_cast({decode, Data}, State = #state{bridge = Core}) ->
    gen_server:cast(Core, jiffy:decode(Data)),
    {noreply, State}.

handle_info({_C, _Info}, State = #state{connection = _C, bridge = Bridge}) ->
    Bridge ! {self(), _Info},
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

-spec parse_json(binary()) -> json().
parse_json(Binary) ->
    jiffy:decode(Binary).

