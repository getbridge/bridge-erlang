-module(bridge_event).
-behaviour(gen_event).

-export([start_link/1, init/1, code_change/3, terminate/2]).
-export([handle_call/2, handle_event/2, handle_info/2]).


start_link(Type) ->
  gen_event:start_link(Type).

init(_Options) ->
  ok.

handle_call(_Request, _State) ->
  ok.
handle_event(_Request, _State) ->
  ok.
handle_info(_Request, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  State.
terminate(_Reason, _State) ->
  ok.
