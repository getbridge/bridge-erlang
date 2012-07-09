-module(bridge.system).

-export([hookChannelHandler/2, hookChannelHandler/3]).

-export([getService/2]).

-export([remoteError/1]).

hookChannelHandler(Name, Handler) ->
    hookChannelHandler(Name, Handler, undefined).

hookChannelHandler(_Name, _Handler, _Callback) ->
    ok.

getService(_Name, _Func) ->
    ok.

remoteError(_A) ->
    ok.
