-module(bridge.event).

-behaviour(gen_event).

-export([code_change/3, init/1, start_link/1,
	 terminate/2]).

-export([handle_call/2, handle_event/2, handle_info/2]).

-export([notify/3]).

start_link(Type) -> gen_event:start_link(Type).

init(_Options) -> ok.

handle_call(_Request, _State) -> ok.

handle_event(_Request, _State) -> ok.

handle_info(_Request, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> State.

terminate(_Reason, _State) -> ok.

notify(Atom, M, {E, D, R}) ->
    notify(case Atom of
	     error -> E;
	     disconnect -> D;
	     reconnect -> R;
	     _ -> throw("Unexpected atom")
	   end,
	   M).

notify(Handler, Msg) ->
    gen_event:notify(Handler, Msg).
