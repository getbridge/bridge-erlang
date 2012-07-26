-module(bridge_encoder).
-behaviour(gen_server).

%% Presentation layer.

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([code_change/3, terminate/2]).
-export([start_link/1]).

-export([parse_json/1]).

-record(state,
        { connection = undefined,
          bridge     = undefined
        }).

-spec start_link(bridge:options()) -> {ok, pid()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

-spec init({bridge:options(), pid()}) -> {ok, #state{}}.
init(_Args = {Opts, Parent}) ->
    {ok, Conn} = bridge_connection:start_link(Opts),
    {ok, #state{bridge     = Parent,
                connection = Conn}}.

handle_call(_Args, _From, State) ->
    {noreply, State}.

-spec handle_cast({connect, bridge:json_obj()}, #state{}) ->
                         {noreply, #state{}};
                 ({connect_response, {binary(), binary()}}, #state{}) ->
                         {noreply, #state{}};
                 ({encode, {bridge:command(), bridge:json_obj()}}, #state{}) ->
                         {noreply, #state{}};
                 ({decode, binary()}, #state{}) ->
                         {noreply, #state{}}.
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

-spec handle_info({pid(), {atom(), term()}}, #state{}) -> {noreply, #state{}}.
handle_info({C, {Tag, Info}}, State = #state{connection = C, bridge = Core}) ->
    Core ! {self(), {Tag, Info}},
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

-spec parse_json(binary()) -> bridge:json().
parse_json(Binary) ->
    jiffy:decode(Binary).
