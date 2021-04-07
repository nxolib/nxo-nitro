-module(nxo_route_handler).
-behaviour(route_handler).
-include("nxo.hrl").

-export([
         init/2,
         finish/2
        ]).

init(Config, State) ->
  Bridge = wf_context:bridge(),
  Path = sbw:path(Bridge),
  case string:prefix(Path, "/api/") of
    nomatch ->
      do_default_init(Config, State);
    APIReq ->
      do_api_init(Config, State, APIReq)
  end.

finish(_Config, State) ->
  {ok, State}.



do_default_init(Config, State) ->
  dynamic_route_handler:init(Config, State).

do_api_init(Config, State, APIReq) ->
  [Version, Action | Info] = string:split(APIReq, "/", all),
  Token = wf:header(authorization),
  case is_valid_token(Token) of
    {true, UserData} ->
      Module = wf:to_atom(string:join(["api", Version, Action], "_")),
      case is_rest_module(Module) of
        true  ->
          case is_valid_content_type() of
            true -> dispatch_to_rest(Module, Info, State, UserData);
            false -> do_api_error(not_acceptable, Config, State,
                                  "invalid or missing content-type")
          end;
        false -> do_api_error(no_resource, Config, State, "invalid resource")
      end;
    false ->
      do_api_error(not_authorized, Config, State, "invalid token")
  end.


dispatch_to_rest(Module, Info, State, UserData) ->
  Entry = fun() -> nitrogen_rest:handle_request(Module) end,
  PathInfo = string:join(Info, "/"),
  wf_context:page_module(Module),
  wf_context:path_info(PathInfo),
  wf_context:entry_point(Entry),
  set_session_info([org_abbrv, org_id, org_name, user_id], UserData),
  nxo_api:log(Module, PathInfo),
  {ok, State}.


do_api_error(ErrorFn, _Config, State, Msg) ->
  wf_context:page_module(api_v1_error),
  wf_context:path_info(Msg),
  wf_context:entry_point(ErrorFn),
  {ok, State}.


is_valid_token(_Token) -> unimplemented.
  %% try string:split(Token, ":") of
  %%   [APIKey, OrgAbbrv] -> nxo_api:login(APIKey, OrgAbbrv)
  %% catch
  %%   _:_ -> false
  %% end.


set_session_info(KeyList, Data) ->
  lists:foreach(
    fun(Key) ->
        Value = maps:get(atom_to_binary(Key, latin1), Data),
        wf:session(Key, Value) end,
    KeyList).

is_rest_module(Module) ->
  wf_utils:has_behaviour(Module, nitrogen_rest).

is_valid_content_type() ->
  lists:member(wf:header('content-type'), ?API_CONTENT_TYPES).
