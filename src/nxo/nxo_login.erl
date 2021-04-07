-module(nxo_login).
-include("nxo.hrl").

-export([
          login/2
        , requires_2fa/1
        ]).

%% @doc Attempt to log into the site.
%%
%% Called, I suppose, from delegates/login.  Returns true if the
%% credentials are acceptable and any other checks (active/inactive,
%% for instance) pass; false otherwise.
%%
%% User can be an email address or SAMAccountName.
-spec login(User :: string(), Pass :: string()) -> {true, map()} | false.

login(User, Pass) ->
  case (is_user_active(nxo_auth_user:find(User))) of
    {true, UserData} -> authenticate_user(UserData, Pass);
    _ -> false
  end.


requires_2fa(UserData) ->
  UserID = maps:get(<<"user_id">>, UserData),
  case nxo_db:scalar_query(user_requires_2fa, [UserID]) of
    0 -> false;
    _ -> true
  end.


%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%

is_user_active([#{<<"active">> := true}=UserData]) ->
  {true, UserData};
is_user_active(_) ->
  false.

authenticate_user(#{ <<"samaccountname">> := SAM }=UserData, Pass)
  when SAM =:= <<>> orelse SAM =:= null ->
  case erlpass:match(Pass, maps:get(<<"password">>, UserData)) of
    true -> successful_audit(UserData);
    false -> failed_audit(UserData, "local password failure")
  end;
authenticate_user(UserData, Pass) ->
  case nxo_ad:authenticate(maps:get(<<"samaccountname">>, UserData), Pass) of
    true -> successful_audit(UserData);
    false -> failed_audit(UserData, "AD password failure")
  end.

successful_audit(#{ <<"user_id">> := UserID }=UserData) ->
  nxo_db:query(user_audit_insert, [UserID, true, "login", ""]),
  {true, UserData}.

failed_audit(#{ <<"user_id">> := UserID }, Comment) ->
  nxo_db:query(user_audit_insert, [UserID, false, "login", Comment]),
  false.
