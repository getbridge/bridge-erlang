-module(bridge_event).
-behaviour(gen_event).

%% Default event handler.

-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

init(_Opts) ->
    {ok, ':)'}.

handle_event({Tag, Event}, State) ->
    io:format("~p : ~p ~n", [Tag, Event]),
    {ok, State}.

handle_info(Info, State) ->
    gen_event:notify(self(), {info, Info}),
    {ok, State}.

handle_call(Msg, State) ->
    {ok, Msg, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
