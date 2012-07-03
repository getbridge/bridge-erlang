-module(bridge_connection).
-behaviour(gen_server).

-export([start_link/1, process_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

start_link(Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

init(Options) ->
  Serializer = bridge_serializer:start_link()
  {ok, serializer}.

process_message(Data) ->
  ok.

handle_cast(Request, State) ->
  
