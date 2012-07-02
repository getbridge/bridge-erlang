-module(bridge_serializer).
-behaviour(gen_server).
-export([serialize/2, unserialize/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

-export([]).
serialize(Data, _Process) ->
  {reply, Reply, _NewState} = call(encode, {Data, _Process}),
  jiffy:encode(Reply).
unserialize(Data, _Process) ->
  {reply, Reply, _NewState} = call(decode, {Data, _Process}),
  jiffy:decode(Reply).

init(_Args) ->
  {ok, dict:new()}.

call(Token, _Args = {_Data, _Process}) ->
  gen_server:call(_Process, {Token, _Data}).

handle_call({encode, Data}, From, State) ->
  {reply, Reply, NewState};
handle_call({decode, Data}, From, State) ->
  {reply, Reply, NewState}.
