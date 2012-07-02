-module(bridge_event).
-behaviour(gen_event).

-export([init/1, code_change/3, terminate/2]).
-export([handle_call/2, handle_event/2, handle_info/2]).
