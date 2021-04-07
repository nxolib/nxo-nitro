-module(page_login).
-include("nxo.hrl").
-export([
          event/1
        , main/0
        , title/0
        , body/0
        , set_user/1
        , set_user/2
        ]).

-security(none).
-postback_security(none).

main() -> #template{ file=nxo:template("login.html") }.

title() -> "Login".

body() ->
  case wf:path_info() of
    "logout" -> event(logout);
    "unmask" -> event(unmask);
    _ -> case wf:user() of
           undefined -> show_content();
           _         -> wf:redirect("/")
         end
  end.

event(login) ->
  [Username, Password] = wf:mq([username, password]),
  case nxo_login:login(Username, Password) of
    {true, UserData} ->
      Requires2FA = set_user(UserData),
      case Requires2FA of
        true  -> wf:redirect("/duoweb_login");
        false -> wf:redirect_from_login("/")
      end;
    false ->
      wf:redirect_to_login("/login")
  end;

event(unmask) ->
  case nxo_auth_user:find(wf:session(pre_masquerade_user)) of
    [UserData] -> set_user(UserData),
                  wf:session(pre_masquerade_user, undefined),
                  wf:redirect("/");
    _ -> ok
  end;

event(logout) ->
  nxo_sessions:kill(wf:user()),
  nxo_db:query(user_audit_insert, [wf:user(), true, "logout", ""]),
  wf:logout(),
  wf:redirect("/").

set_user(UserData) ->
  set_user(UserData, false).

set_user(UserData, Masquerade) ->
  Requires2FA = nxo_login:requires_2fa(UserData),
  UserID = maps:get(<<"user_id">>, UserData),
  MaybeUser = wf:user(),
  wf:clear_session(),
  wf:session(display_name, display_name(UserData, Masquerade)),
  case Masquerade of
    true -> wf:session(pre_masquerade_user, MaybeUser);
    false -> ok
  end,
  wf:session(user_data, UserData),
  case Requires2FA andalso not Masquerade of
    false -> wf:user(UserID);
    true  -> wf:session(maybe_user, UserID)
  end,
  Requires2FA.

display_name(UserData, false) ->
  display_name(UserData);
display_name(UserData, true) ->
  display_name(UserData) ++ " [mask]".

display_name(UserData) ->
  io_lib:format("~s ~s",
                [maps:get(<<"first_name">>, UserData),
                 maps:get(<<"last_name">>, UserData)]).



%% INTERNAL FUNCTIONS

show_content() ->
  case wf:session(login_failure) of
    undefined -> ok;
    Message   -> wf:insert_before(defer, login_panel, message_box(Message))
  end,
  #panel{
     id=login_panel,
     class="mt-4 ml-4 form-group",
     body=[
           #panel{ class="row",
                   body=[
                         #panel{
                            class="col-sm-5",
                            body=[ #textbox{ id=username,
                                             placeholder="Username",
                                             next=password,
                                             class="form-control" } ]},
                         #panel{
                            class="col-sm-5",
                            body=[ #password{ id=password,
                                              placeholder="Password",
                                              class="form-control",
                                              postback=login} ]},
                         #panel{
                            class="col-sm-2",
                            body=[ #button{ text="Sign In",
                                            id=login_button,
                                            class="btn btn-block btn-success",
                                            postback=login} ]} ]} ]}.

message_box(Message) ->
  #panel{ class="alert alert-secondary", text=Message }.
