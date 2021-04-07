-module(api_v1_test).
-behaviour(nitrogen_rest).
-include("nxo.hrl").

-export([ get/1
        , post/1
        , put/1
        , delete/1 ]).

get(_) ->
  case nxo:content_type() of
    xml  ->
      wf:content_type("application/xml"),
      "<{{name}}><result>success</result><version>1</version></{{name}}>";
    json ->
      wf:content_type("application/json"),
      wf:json_encode([{result, success}, {version, 1}])
  end.

post(_) -> ok.
put(_) -> ok.
delete(_) -> ok.
