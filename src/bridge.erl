-module(bridge).

-export([new/1, connect/1]).
-export([on_error/2, on_disconnect/2, on_reconnect/2]).

-export([publish_service/3, publish_service/4]).
-export([join_channel/3, join_channel/4, join_channel/5]).
-export([get_service/2, get_channel/2]).

-export([get_client/2, context/1]).

-record(bridge, {
    on_err	= [],
    on_disconn	= [],
    on_reconn	= [],
    opts	= [],
    context	= undefined,
    conn	= undefined,
    refs        = undefined % avl_tree
  }).

DefaultOptions = [{log, bridge_logging:WARNING},
		  {redirector, "http://redirector.getbridge.com"},
		  {secure_redirector, "https://redirector.getbridge.com"},
		  {secure, true}].

is_proplist(Lst) ->
  is_list(Lst) andalso
    lists:all(
      fun(X) ->
	  (is_tuple(X) andalso
	   size(X) == 2)
      end,
      Lst).

new(Options) ->
  case is_proplist(Options) of
    true ->
      #bridge{opts = Options ++ DefaultOptions};
    false ->
      bridge_logging:err("Could not process options provided.")
  end.

connect(Bridge) ->
  Bridge#bridge{conn=bridge_conn:new(Bridge#bridge.opts)}.

on_error(Bridge = #bridge{}, Callback) ->
  Bridge#bridge{on_err = Bridge#bridge.on_err ++ [Callback]}.
on_disconnect(Bridge = #bridge{}, Callback) ->
  Bridge#bridge{on_disconn = Bridge#bridge.on_disconn ++ [Callback]}.
on_reconnect(Bridge = #bridge{}, Callback) ->
  Bridge#bridge{on_reconn = Bridge#bridge.on_reconn ++ [Callback]}.

publish_service(Bridge, SvcName, Handler) ->
  publish_service(Bridge, SvcName, Handler, undefined).
publish_service(Bridge, SvcName, Handler, Callback) ->
  send(). % TODO: IMPLEMENT

join_channel(Bridge, ChName, Handler) ->
  join_channel(Bridge, ChName, Handler, true).
join_channel(Bridge, ChName, Handler, Writeable) when is_boolean(Writeable) ->
  join_channel(Bridge, ChName, Handler, Writeable, undefined);
join_channel(Bridge, ChName, Handler, Callback) ->
  join_channel(Bridge, ChName, Handler, true, Callback).
join_channel(Bridge, ChName, Handler, Writeable, Callback) ->
  ok. % TODO: IMPLEMENT

get_service(Bridge, SvcName) ->
  ok. % TODO: IMPLEMENT

get_channel(Bridge, ChName) ->
  ok. % TODO: IMPLEMENT

context(Bridge) ->
  Bridge#bridge.context.

get_client(Bridge, ClientId) ->
  bridge_client:new(Bridge, ClientId).


send(Bridge, Plist) ->
  bridge_conn:send(bridge_serializer:serialize(bridge, Plist)).
