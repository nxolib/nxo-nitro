-module({{name}}).
-export([
          db_pass/1
        ]).

db_pass(_Pool) ->
  "{{name}}_pass".
