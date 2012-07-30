-module(server).
-behaviour(gen_server).

-import(bridge).

-import(erlang).

-import(gen_server).
-import(io).

-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).

-export([main/0, ping/1]).

-export([exports/0]).

-record(state, { bridge = undefined }).

exports() ->
    [message].

-spec start_link(pid()) -> {ok, pid()}.
start_link(Bridge) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Bridge, []).

ping(_State = #state{bridge = B}) ->
    io:format("PING!~n"),
    Caller = bridge:get_client(B, bridge:context(B)),
    Pong = bridge:get_service(B, {Caller, pong}),
    bridge:cast(B, {Pong, pong, []}).

main() ->
    {ok, Bridge} = bridge:new([{api_key, 'myapikey'},
                               {secure, true}]),
    %% bridge:connect(Bridge),
    {ok, PingHandler} = ?MODULE:start_link(Bridge),
    bridge:publish_service(Bridge, {ping, PingHandler}),
    bridge:connect(Bridge),
    ok.

init(Bridge) ->
    {ok, #state{bridge = Bridge}}.


handle_cast({Method, Args}, State) ->
    erlang:apply(?MODULE, Method, Args ++ [State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(exports, _From, State) ->
    {reply, exports(), State};
handle_call(_Message, _From, State) ->
    {reply, okay, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
