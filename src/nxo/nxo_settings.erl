-module(nxo_settings).
-include("nxo.hrl").
-export([
          get/1
        , get/2
        , get/3
        , set/3
        , groups/0
        ]).

%% @doc Return all of the setting_group (e.g., `<<"ad">>') and
%% setting_group_labels (e.g. `<<"AD Settings">>') as a map.
-spec groups() -> map().
groups() ->
  lists:foldl(fun(#{ <<"setting_group">> := Group,
                     <<"setting_group_label">> := Label }, MapAcc) ->
                  maps:put(Group, Label, MapAcc)
              end,
              #{},
              nxo_db:map_query(settings_get_groups, [])).

%% @doc Get all the settings for a group.  Only pulls from the DB.
-spec get(atom()) -> map().
get(Group) ->
  nxo_db:map_query(settings_get_group, [Group]).

%% @doc Get a specific setting.  Attempt to pull from the DB, if the
%% value isn't present, try the config file with ?APP/Name; if that
%% fails, return undefined.
-spec get(atom(), atom()) -> any() | undefined.
get(Group, Name) ->
  case nxo_db:scalar_query(settings_get_item, [Group, Name]) of
    null  -> application:get_env(?APP, Name, undefined);
    Value -> Value
  end.

%% @doc Get a specific setting or the supplied default.  Only looks in
%% the DB (unlike get/2).
-spec get(atom(), atom(), any()) -> any().
get(Group, Name, Default) ->
  case nxo_db:scalar_query(settings_get_item, [Group, Name]) of
    null  -> Default;
    Value -> Value
  end.

%% @doc Set a setting!
-spec set(atom(), atom(), any()) -> ok.
set(Group, Name, Value) ->
  OldValue = get(Group, Name),
  nxo_db:query(settings_audit_insert,
                [Group, Name, user_id(), OldValue, Value]),
  {ok, 1} = nxo_db:query(settings_set_item, [Group, Name, Value]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% If we're testing from the console whomp up a fake user_id (since
%% there's no WF context to speak of) but only in development.
user_id() ->
  try wf:user() of
      UserID -> UserID
  catch
    _:Reason -> case nxo:is_development() of
                  true  -> uuid:to_string(uuid:uuid4());
                  false -> error(Reason)
                end
  end.
