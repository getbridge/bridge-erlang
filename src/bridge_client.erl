-module(bridge_client).
-export([new/1, get_service/2]).

new(ClientId) ->
  ["client", ClientId].

get_service(SvcName, Client) ->
  Client ++ [SvcName].
