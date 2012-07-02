-module(bridge_connection).
-behaviour(gen_server).

-export([start_link/1, process_message/1]).

record(bridge_conn, {
    conn,
    queue
  }).

start_link(Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

init(Options) ->
  ok.

process_message(Data) ->
  ok.
