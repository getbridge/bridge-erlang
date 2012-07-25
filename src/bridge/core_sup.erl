-module(bridge.core_sup).
-behaviour(supervisor).

-export([init/1]).

init(Arg) ->
    {ok, {{one_for_all, 1, 30},
          [{ core, {bridge.core, start_link, [Arg]},
             transient, 1000, worker, [bridge.core] }]}}.
