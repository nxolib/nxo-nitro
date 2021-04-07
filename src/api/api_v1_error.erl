-module(api_v1_error).
-include("nxo.hrl").

-export([
          not_authorized/0
        , no_resource/0
        , not_acceptable/0
        ]).

%% Called by nxo_route_handler when something can't be dispatched.

no_resource() ->
  wf:status_code(404),
  jsx:encode(#{ <<"status">> => 404,
                <<"userMessage">> => <<"Resource not found.">>,
                <<"developerMessage">> => <<"not found">>,
                <<"debugInfo">> => wf:to_binary(wf:path_info()) }).

not_authorized() ->
  wf:status_code(401),
  jsx:encode(#{ <<"status">> => 401,
                <<"userMessage">> => <<"Not authorized.">>,
                <<"developerMessage">> => <<"not authorized">>,
                <<"debugInfo">> => wf:to_binary(wf:path_info()) }).

not_acceptable() ->
  wf:status_code(406),
  jsx:encode(#{ <<"status">> => 406,
                <<"userMessage">> => <<"Invalid content-type.">>,
                <<"developerMessage">> => <<"invalid content-type">>,
                <<"debugInfo">> => wf:to_binary(wf:path_info()) }).
