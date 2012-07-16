-module(chatclient).
-behaviour(gen_server).

-import(bridge).

-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).

-export([main/0, message/3]).

-record(state, { bridge = undefined }).

start_link(Bridge) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Bridge, []).

message(Sender, Message, _State) ->
    io:format("~p: ~p~n", [Sender, Message]).

join_callback(Channel, Name, Bridge) -> 
    io:format("Joined channel : ~p~n", [Name]),
    bridge:cast(Bridge, {Channel,
                         message,
                         [steve, <<"Bridge is pretty nifty">>]}).

main() ->
    {ok, Bridge} = bridge:new([{api_key, '951da7fb819d0ef3'},
                               {secure, false}]),
    %% bridge:connect(Bridge),
    {ok, ChatHandler} = chatserver:start_link(Bridge),
    .io:format("hello, testing~n"),
    Auth = bridge:get_service(Bridge, auth),
    bridge:cast(Bridge, {Auth, join, ['bridge-lovers',
                                      'secret123',
                                      ChatHandler,
                                      fun join_callback/3]}).

init(Bridge) ->
    {ok, #state{bridge = Bridge}}.


handle_cast({Method, Args}, State) ->
    apply(?MODULE, Method, Args ++ [State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(_Message, _From, State) ->
    {reply, okay, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
