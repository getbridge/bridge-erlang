-module(bridge).
-behaviour(gen_server).

% Gen server API, which should be familiar to Erlang speakers.
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([cast/2]).
-export([code_change/3, terminate/2]).

% Bridge API, which is foreign to Erlang speakers, but convenient wrapper
% for gen_server:

-export([new/1, connect/1]).
-export([on_error/2, on_disconnect/2, on_reconnect/2]).

-export([publish_service/3, publish_service/4]).
-export([join_channel/3, join_channel/4, join_channel/5]).
-export([get_service/2, get_channel/2]).

-export([get_client/2, context/1]).

-record(bridge, {opts			= [],
		 connection_handler	= undefined,
		 error_handler		= undefined,
		 disconnect_handler	= undefined,
		 reconnect_handler	= undefined,
		 context		= undefined
	       }).

start_link(Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

init(Options) ->
  case is_proplist(Options) of
    true ->
      Opts = Options ++
	[{log, 2},
	 {redirector, "http://redirector.getbridge.com"},
	 {secure_redirector, "https://redirector.getbridge.com"},
	 {secure, true}],
      {ok, #bridge{
	 opts = Opts,
	 connection_handler = bridge_connection:start_link(Opts),
	 error_handler = bridge_event:start_link(),
	 disconnect_handler = bridge_event:start_link(),
	 reconnect_handler = bridge_event:start_link()
	}
      };
    false ->
      bridge_logging:err("Options must be a proplist."),
      {error, "Options must be a proplist."}
  end.

handle_call(_Request, _From, _State) ->
  {error, "Synchronous support not yet added."}.

handle_cast(_Request, _State) ->
  {error, "Asynchronous support not yet added."}.

handle_info(Info, {ok, #bridge{
		     error_handler = Err,
		     disconnect_handler = Disconn,
		     reconnect_handler = Reconn
		    }
		  }) ->
  case Info of
    {error, _Msg} ->
      bridge_event:notify(Info, Err);
    {disconnect, _Msg} ->
      bridge_event:notify(Info, Disconn);
    {reconnect, _Msg} ->
      bridge_event:notify(Info, Reconn)
  end.

cast(Server, {Method, Args}) ->
% Example usage: bridge:cast(get_service("auth"), {Method, Args})
  send('SEND', [{destination, Server ++ [Method]}, {args, Args}]).

on_error(Callback, _State = {ok, #bridge{error_handler = Err}}) ->
  bridge_event:add_handler(Err, Callback).
on_disconnect(Callback, _State = {ok, #bridge{disconnect_handler = Disc}}) ->
  bridge_event:add_handler(Disc, Callback).
on_reconnect(Callback, _State = {ok, #bridge{reconnect_handler = Reconn}}) ->
  bridge_event:add_handler(Reconn, Callback).

send(Command, Data, _State = {ok, #bridge{connection_handler = Conn}}) ->
  gen_server:cast(Conn, {Command, Data}).


is_proplist(Lst) when is_list(Lst) ->
  lists:all(fun(X) -> is_tuple(X) andalso size(X) == 2 end, Lst);
is_proplist(_) ->
  false.

new(Opts) ->
  start_link(Opts).

connect(State) ->
  bridge_conn:connect(State).

publish_service(SvcName, Handler, State) ->
  publish_service(SvcName, Handler, undefined, State).
publish_service(SvcName, Handler, Callback, State) ->
  send(publish_service,
       [{name, SvcName},
	{handler, Handler},
	{callback, Callback}],
       State).

join_channel(ChName, Handler, State) ->
  join_channel(ChName, Handler, true, State).
join_channel(ChName, Handler, Writeable, State) when is_boolean(Writeable) ->
  join_channel(ChName, Handler, Writeable, undefined, State);
join_channel(ChName, Handler, Callback, State) ->
  join_channel(ChName, Handler, true, Callback, State).
join_channel(ChName, Handler, Writeable, Callback, State) ->
  send(join_channel,
       [{name, ChName},
	{handler, Handler},
	{callback, Callback},
	{writeable, Writeable}],
       State).

get_service(SvcName, State) ->
  get_path_chain(SvcName). % TODO: IMPLEMENT

get_channel(ChName, State) ->
  ok. % TODO: IMPLEMENT

context({ok, #bridge{context=Context}}) ->
  Context.

get_client(ClientId, State) ->
  bridge_client:new(ClientId, State).
