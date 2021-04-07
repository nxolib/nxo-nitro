-module(nxo).
-include("nxo.hrl").
-export([
          template/1
        , application/0
        , auth_template/1
        , authz_template/2
        , is_development/0
        , version/0
        , version/1
        , keys_to_binary/1
        , fa/1
        , clear_sql_cache/0
        , content_type/0
        , xsd_file/1
        , uuid/0
        , is_uuid/1
        , is_string/1
        , is_real_list/1
        , global_auth_enabled/0
        , global_auth_allowed/0
        , event_handler/0
        , notify/1
        , user/0
        , is_authenticated/0
        , context_safe/3
        , user_token/0
        , pickle/1
        , depickle/1
        , url_path/1
        ]).

-spec application() -> atom().
application() ->
    {ok, App} = application:get_application(?MODULE),
    App.

-spec template(file:name_all()) -> file:filename_all().
template(File) ->
  nxo_template_name_cache:lookup(File).

-spec auth_template(File :: file:name_all()) -> file:filename_all().
auth_template(File) ->
  case wf:user() of
    undefined -> wf:redirect_to_login("/login");
    _         -> #template{ file=?MODULE:template(File) }
  end.

-spec authz_template(Role :: atom(), File :: file:name_all()) ->
                        file:filename_all().
authz_template(Role, File) ->
  case wf:user() of
    undefined -> wf:redirect_to_login("/login");
    _ ->
      case nxo_authz:may(Role) of
        true  -> #template{file=?MODULE:template(File)};
        false -> wf:redirect("/not_authorized")
      end
  end.

-spec is_development() -> boolean().
is_development() ->
  application:get_env(?APP, development_mode, false).

-spec version() -> string().
version() -> version(application()).

-spec version(Application :: atom()) -> string().
version(Application) ->
  {_, _, V} = lists:keyfind(Application, 1, application:loaded_applications()),
  V.

%% @doc Returns the map with the keys normalized to binary.  This is
%% particularly useful for matching against data returned from
%% postgres.
-spec keys_to_binary(Map :: map()) -> map().
keys_to_binary(Map) ->
  Fun = fun(K, V, NewMap) -> maps:put(wf:to_binary(K), V, NewMap) end,
  maps:fold(Fun, #{}, Map).

%% @doc Returns the Font Awesome HTML for the specified glyph.  The
%% glyph may be specified as either a string or atom and should not
%% include the 'fa-' prefix.
%% @param Icon the Font Awesome glyph to render.
-spec fa(Icon :: string() | atom()) -> string().
fa(Icon) when is_atom(Icon) -> fa(atom_to_list(Icon));
fa(Icon)                    ->  "<i class=\"fa fa-fw fa-" ++ Icon ++ "\"></i>".

%% @doc Clears the SQL cache.
-spec clear_sql_cache() -> ok.
clear_sql_cache() -> nxo_sql_cache:flush().

%% @doc Returns a simple content type.
-spec content_type() -> xml | json | other.
content_type() ->
  case wf:header('content-type') of
    "application/json" -> json;
    "application/xml"  -> xml;
    _                  -> other
  end.

%% @doc Return an xsd filepath.
-spec xsd_file(string()|binary()|atom()) -> file:filename_all().
xsd_file(File) ->
  filename:join([?XSD_DIR, wf:to_list(File) ++ ".xsd"]).

%% @doc Return a UUIDv4 as a string.
uuid() ->
  uuid:to_string(uuid:uuid4()).

%% @doc Return true if a valid uuid, false otherwise.
is_uuid(UUID) ->
  try uuid:is_valid(wf:to_list(UUID)) of
      true -> true;
      false -> false
  catch
    _:_ ->
       false
  end.

%% @doc Return true if the list is a string, false otherwise.
is_string(S) when is_list(S) ->
  lists:all(fun(X) -> is_integer(X) end, S);
is_string(_) ->
  false.

%% @doc Return true if the value is a list (but not a string).
%%
%% There's one real failing here: lists of integers ([1,2,3]) cannot
%% be differentiated from strings (ha!) so user beware.
is_real_list(V) when is_list(V) ->
  not is_string(V);
is_real_list(_) ->
  false.

%% @doc Returns true if global_auth_required is true; false otherwise.
%%
%% When global_auth_required is true, all users must authenticate.
%% This is a tool for development or pre-release.
-spec global_auth_enabled() -> true | false.
global_auth_enabled() ->
  application:get_env(application(), global_auth_required, false).


%% @doc Returns true if access to the resource is not hindered by
%% global_auth.  This is primarily for use with events (see nxo_db).
-spec global_auth_allowed() -> true | false.
global_auth_allowed() ->
  %% check for websocket request
  Context = wf_context:context(),
  WS = case Context#context.type of
         postback_websocket -> true;
         _                  -> false
       end,
  case {global_auth_enabled(), wf:user(), wf:page_module(), WS} of
    {_,     _, login, false} -> true;                  % access to login
    {false, _, _    , _    } -> true;                  % no global turned on
    {_,     U, _    , _    } when U =/= undefined -> true; % user logged in
    {_,     _, _    , _    } -> false
  end.

%% @doc Returns the PID of the NXO event handler.
-spec event_handler() -> pid().
event_handler() ->
  nprocreg:get_pid(?EVENT).

%% @doc Send an event notification to the NXO event handler.
-spec notify(Msg :: any()) -> ok.
notify(Msg) ->
  gen_event:notify(event_handler(), Msg).

%% @doc Safely execute wf:user(); return username or 'undefined'.
%%
%% wf:user() requires a context; we might want to check for a user in
%% code that is not a page request (e.g., for logging).  This function
%% allows that to happen without an error.
-spec user() -> any() | undefined.
user() ->
  context_safe(fun wf:user/0, [], undefined).

%% @doc Test if user is authenticated.
-spec is_authenticated() -> boolean().
is_authenticated() ->
  not(user() == undefined).

%% @doc Provide a way of executing WF functions without a context.
%% Returns the value of the fun() or NoCtx if an exception is thrown.
-spec context_safe(function(), Args :: [term()], NoCtx :: any()) -> any().
context_safe(Fn, Args, NoCtx) ->
  try apply(Fn, Args) of
    Result -> Result
  catch
    _:_ -> NoCtx
  end.

%% @doc Return an appropriate user token.
%%
%% In the case of a logged in user, this will be the (binary) UserID.
%% An unauth'd web request will return the (binary) session_id.
%% A non-request will return a (binary) reference.
-spec user_token() -> binary().
user_token() ->
  case wf:in_request() of
    true ->
      case wf:user() of
        undefined -> wf:session_id();
        User      -> User
      end;
    false ->
      list_to_binary(ref_to_list(make_ref()))
  end.

%% @doc Encode a term suitable for DB storage.
-spec pickle(term()) -> base64:ascii_binary().
pickle(Term) ->
  base64:encode(term_to_binary(Term)).

%% @doc Decode a pickled payload and return a term.
-spec depickle(base64:ascii_binary()) -> term().
depickle(PickledTerm) ->
  binary_to_term(base64:decode(PickledTerm)).

%% @doc Create a URL path from the list of terms.  We'll add the
%% leading slash and de-duplicate extra slashes as well.
-spec url_path([term()]) -> string().
url_path(Parts) ->
  String = string:join(["/" | [ wf:to_list(P) || P <- Parts ]], "/"),
  re:replace(String, "//+", "/", [global, {return,list}]).
