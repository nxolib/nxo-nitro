-module({{name}}).
-export([
          db_pass/1
        ]).

db_pass(_Pool) ->
  {ok, PassSpec} = application:get_env({{name}}, db_pass),
  case filelib:is_file(PassSpec) of
    true ->
      {ok, PW} = file:read_file(PassSpec),
      PW;
    false ->
      PassSpec
  end.
