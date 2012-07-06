-module(bridge_system).
-export([hookChannelHandler/3, hookChannelHandler/2]).
-export([getService/2]).
-export([remoteError/1]).

hookChannelHandler(Name, Handler) ->
  hookChannelHandler(Name, Handler, undefined).
hookChannelHandler(_Name, _Handler, _Callback) ->
  ok.

getService(Name, Func) ->
  ok.

remoteError(A) ->
  ok.
