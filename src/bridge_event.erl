-module(bridge_event).
-behaviour(gen_event).

%% Default event handler.

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-spec init(bridge:options()) -> {ok, 0..3}.
init(_Opts) ->
    LogLevel = proplists:get_value(log, _Opts),
    {ok, LogLevel}.

handle_event({Tag, Event}, State) ->
    case should_log(Tag, State) of
        true ->
            io:format("~p : ~p ~n", [Tag, Event]);
        false ->
            ok
    end,
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

should_log(error, State) ->
    State > 0;
should_log(warning, State) ->
    State > 1;
should_log(_Tag, State) ->
    State > 2.
