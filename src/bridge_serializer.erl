-module(bridge_serializer).
-behaviour(gen_server).
-export([serialize/2, unserialize/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

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
  {reply, _Reply, _NewState} = encode(Data, State);
handle_call({decode, Data}, From, State) ->
  {reply, _Reply, _NewState} = decode(Data, State).

decode(Data, State) when is_list(Data) ->
  {Reply, State} = map(fun decode/3, [Data, From, State]).
decode(Data, State) ->
  ok.
encode(Data, From, State) ->
  {Reply, State} = map(fun encode/3, [Data, From, State]).
encode(Data, State) ->
  ok.

handle_cast(_Request, State) ->
  State.
handle_info(_Request, State) ->
  State.
code_change(_OldVsn, State, _Extra) ->
  State.
terminate(_Reason, _State) ->
  ok.
