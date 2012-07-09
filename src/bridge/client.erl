-module(bridge.client).

-export([get_service/2, new/1]).

new(ClientId) ->
    ["client", ClientId].

get_service(Client, SvcName) ->
    Client ++ [SvcName].
