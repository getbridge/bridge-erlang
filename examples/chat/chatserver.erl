-module(chatserver).
-behaviour(gen_server).

-import(bridge).

-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).

-export([main/0, join/5, exports/0]).

-record(state, { bridge = undefined }).

exports() ->
    [join].

start_link(Bridge) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Bridge, []).

join(Room, Password, Obj, Callback, #state{bridge = Bridge}) ->
    if Password == <<"secret123">> ->
            io:format("Welcome!~n"),
            bridge:join_channel(Bridge, {Room, Obj, true, Callback});
       true ->
            io:format("Sorry!~n")
    end.

main() ->
    {ok, Bridge} = bridge:new([{api_key, '951da7fb819d0ef3'}, {secure, true}]),
    %% bridge:connect(Bridge),
    {ok, ChatServer} = chatserver:start_link(Bridge),
    %% The atom auth is internally stored as a liststring, either way.
    bridge:publish_service(Bridge, {auth, ChatServer}).

init(Bridge) ->
    {ok, #state{bridge = Bridge}}.

handle_cast({Method, Args}, State) ->
    apply(?MODULE,
          Method,
          Args ++ [State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(exports, _From, State) ->
    {reply, exports(), State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
