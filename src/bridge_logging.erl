-module(bridge_logging).

-export([err/1, err/2]).
-export([info/1, info/2]).
-export([warn/1, warn/2]).

info(A) ->
  error_logger:info_msg(A);
info(A, B) ->
  error_logger:info_msg(A, B).

warn(A) ->
  error_logger:warn_msg(A);
warn(A, B) ->
  error_logger:warn_msg(A, B).

err(A) ->
  error_logger:error_msg(A);
err(A, B) ->
  error_logger:error_msg(A, B).
