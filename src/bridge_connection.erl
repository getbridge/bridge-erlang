-module(bridge_connection).
-behaviour(gen_server).

-export([start_link/1, process_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).


get_value(Key, PList) ->
  proplists:get_value(Key, PList).

start_link(Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

init(Options) ->
  inets:start(),
  Serializer = bridge_serializer:start_link(),
  case get_value(secure, Options) of
    true ->
      Opts = [{redirector, get_value(secure_redirector, Options)} | Options];
    _ ->
      Opts = Options
  end,
  case dispatch(Opts) of
    {ok, Conn} ->
      {ok, Conn, Serializer};
    Msg ->
      {error, {redirector, Msg}}
  end.

% Assuming Options is a proplist, still: will simply crash otherwise.
dispatch(Opts) ->
  case {get_value(host, Opts), get_value(port, Opts)} of
    {undefined, _} ->
      redirector(Opts);
    {_, undefined} ->
      redirector(Opts);
    {Host, Port} ->
      connect(Host, Port)
  end.

redirector(Opts) ->
  RedirectorUrl = get_value(redirector, Opts),
  ApiKey = get_value(api_key, Opts),
  Target = RedirectorUrl ++ "/redirect/" ++ ApiKey,
  redirector_response(httpc:request(Target)).

redirector_response({ok, {{_Vsn, 200, _Reason}, _Hd, Body}}) ->
  case get_value("data", bridge_serializer:parse_json(Body)) of
    undefined ->
      {error, Body};
    Data ->
      connect(get_value("bridge_host", Data), get_value("bridge_port", Data))
  end;
redirector_response(_Res) ->
  {error, _Res}.

connect(Host, Port) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, [binary]),
  Sock.

handle_cast({Command, Data}, _State = {ok, GatewaySock, Serializer}) ->
  M = bridge_serializer:call(Serializer, [{command, Command}, {data, Data}]),
  gen_tcp:send(GatewaySock, M);
handle_call(_Request, _From, _State) ->
  ok.

handle_info(Request, State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  State.
terminate(_Reason, _State) ->
  ok.


process_message(Data) ->
  ok.
