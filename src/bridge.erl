-module(bridge).
-behaviour(gen_server).

%% 

% Gen server API, which should be familiar to Erlang speakers.
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([cast/3]).
-export([code_change/3, terminate/2]).

% Bridge API, which is foreign to Erlang speakers, but convenient wrapper
% for gen_server:

-export([new/1, connect/1]).
-export([on_error/2, on_disconnect/2, on_reconnect/2]).

-export([publish_service/3, publish_service/4]).
-export([join_channel/3, join_channel/4, join_channel/5]).
-export([get_service/2, get_service/3, get_channel/2]).
-export([leave_service/3, leave_service/4]).
-export([leave_channel/3, leave_channel/4]).

-export([get_client/2, context/1]).

-record(bridge, {
	  opts			= [],
	  connection_handler	= undefined,
	  error_handler		= undefined,
	  disconnect_handler	= undefined,
	  reconnect_handler	= undefined,
	  context		= undefined
	 }).

start_link(Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

init(Options) ->
  Opts = Options ++
    [{log, 2},
     {redirector, "http://redirector.getbridge.com"},
     {secure_redirector, "https://redirector.getbridge.com"},
     {secure, true}],
  {ok, #bridge{
     opts = Opts,
     connection_handler = bridge_connection:start_link({Opts, self()}),
     error_handler = bridge_event:start_link(error),
     disconnect_handler = bridge_event:start_link(disconnect),
     reconnect_handler = bridge_event:start_link(reconnect)
    }
  }.

handle_call(_Request, _From, _State) ->
  {error, "Synchronous support not yet added."}.
handle_cast(_Request, _State) ->
  {error, "Asynchronous support not yet added."}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

cast(Server, {Method, Args}, #bridge{connection_handler = Conn}) ->
% Example: bridge:cast(get_service("auth"), {join, Args = [term()]}, Bridge)
  bridge_connection:cast(Conn, {'SEND', [{ref, Server ++ [Method]},
					 {args, Args}]}).

handle_info({error, M}, S = #bridge{error_handler = E}) ->
  bridge_event:notify(E, M),
  {noreply, S};
handle_info({disconnect, M}, S = #bridge{disconnect_handler = D}) ->
  bridge_event:notify(D, M),
  {noreply, S};
handle_info({reconnect, M}, #bridge{reconnect_handler = R}) ->
  bridge_event:notify(R, M),
  {noreply, S}.

on_error(Callback, _State = #bridge{error_handler = E}) ->
  bridge_event:add_handler(E, Callback),
  {ok, _State}.
on_disconnect(Callback, _State = #bridge{disconnect_handler = D}) ->
  bridge_event:add_handler(D, Callback),
  {ok, _State}.
on_reconnect(Callback, _State = #bridge{reconnect_handler = R}) ->
  bridge_event:add_handler(R, Callback),
  {ok, _State}.

new(Opts) ->
  start_link(Opts).

connect(State) ->
  bridge_conn:connect(State).

publish_service(SvcName, Handler, State) ->
  publish_service(SvcName, Handler, undefined, State).
publish_service(SvcName, Handler, Callback,
		#bridge{connection_handler = Conn}) ->
  bridge_connection:cast(Conn, {'JOINWORKERPOOL',
				[{name, SvcName},
				 {handler, Handler},
				 {callback, Callback}]}).

join_channel(ChName, Handler, State) ->
  join_channel(ChName, Handler, true, State).
join_channel(ChName, Handler, Writeable, State) when is_boolean(Writeable) ->
  join_channel(ChName, Handler, Writeable, undefined, State);
join_channel(ChName, Handler, Callback, State) ->
  join_channel(ChName, Handler, true, Callback, State).
join_channel(ChName, Handler, Writeable, Callback,
	     #bridge{connection_handler = Conn}) ->
  bridge_connection:cast(Conn, {'JOINCHANNEL',
				[{name, ChName},
				 {handler, Handler},
				 {callback, Callback},
				 {writeable, Writeable}]}).

leave_service(ChName, Handler, State) ->
  leave_service(ChName, Handler, undefined, State).
leave_service(ChName, Handler, Callback,
	      #bridge{connection_handler = Conn}) ->
  bridge_connection:cast(Conn, {'LEAVEWORKERPOOL',
				[{name, ChName},
				 {handler, Handler},
				 {callback, Callback}]}).

leave_channel(ChName, Handler, State) ->
  leave_service(ChName, Handler, undefined, State).
leave_channel(ChName, Handler, Callback,
	      #bridge{connection_handler = Conn}) ->
  bridge_connection:cast(Conn, {'LEAVECHANNEL',
				[{name, ChName},
				 {handler, Handler},
				 {callback, Callback}]}).

get_service(SvcName, _State) ->
  ["named", SvcName, SvcName].
get_service(SvcName, Client, _State) ->
  Client ++ [SvcName].

get_channel(ChName, #bridge{connection_handler = Conn}) ->
  bridge_connection:cast(Conn, {'GETCHANNEL', [{name, ChName}]}),
  ["channel", ChName, "channel:" ++ ChName].

context(#bridge{context=Context}) ->
  Context.

get_client(ClientId, _State) ->
  bridge_client:new(ClientId).
