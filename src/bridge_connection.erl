-module(bridge_connection).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

-record(state, {
	  serializer	= undefined,
	  socket	= undefined,
	  parent	= undefined,
	  queue         = [], % calls to be flushed upon connection.
	 }).

%% Connection manager.

get_value(Key, PList) ->
  proplists:get_value(Key, PList).

start_link(Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, {Opts, self()}, []).

init({Options, Parent}) ->
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
      {ok, #state{socket=Conn, serializer=Serializer, parent=Parent}};
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

handle_cast({Command, Data}, State = #state{ok, TcpSock, Serializer}) ->
  M = bridge_serializer:call(Serializer,
			     {encode, [{command, Command}, {data, Data}]}),
  {ok, TcpSock} = TcpSock:send(bridge_serializer:serialize(M)),
  State.

handle_call(_Request, _From, _State) ->
  ok.

handle_info({tcp, _Socket, Str}, State) ->
  {noreply, process_message(Str, State)};
handle_info(_Request, _State) ->
  {error, _Request}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

process_message(Msg, State) ->
  M = bridge_serializer:unserialize(Msg),
  bridge_serializer:call(Serializer, {eval, M}),
  {ok, State}.
