-module(gf_app_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([run/1]).

-spec all() -> [atom()].
all() -> [run].

-spec run(proplists:proplist()) -> {comment, []}.
run(_Config) ->
  {ok, _} = gold_fever:start(),
  ok = gold_fever:stop(),
  {comment, ""}.
