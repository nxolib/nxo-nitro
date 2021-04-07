-module(nxo_template_name_cache).
-include("nxo.hrl").

-export([
          init/0
        , lookup/1
        , flush/0
        ]).

-define(CACHE, nxo_template_name_cache).
-define(EXPIRE_TIME, 24 * 60 * 60 * 1000).

init() ->
  nitro_cache:init(?CACHE).

flush() ->
  nitro_cache:flush(?CACHE).

lookup(Template) ->
  nitro_cache:get(?CACHE, ?EXPIRE_TIME, Template,
                  fun() -> find_template(Template) end).

find_template(Template) ->
  wf:info("Recaching Template Name: ~p", [Template]),
  Pattern = filename:join(["**", wf:to_list(Template)]),
  case filelib:wildcard(Pattern, ?HTML_DIR) of
    [] -> error("HTML Template " ++ Template ++ " not found");
    [X | _] -> filename:join([?HTML_DIR, X])
  end.
