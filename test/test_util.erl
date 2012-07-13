-module(test_util).

-export([random_string/2]).

random_string(Len, Chars) ->
    [lists:nth(random:uniform(len(Chars)), Chars) || _X <- lists:seq(Len)].
