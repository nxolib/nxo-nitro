-module(page_duoweb_login).
-include("nxo.hrl").
-export([ main/0
        , title/0
        , body/0
        ]).

-security(none).

main() ->
  case wf:request_method() of
    get -> #template{ file=nxo:template("duoweb_login.html") };
    post -> verify_response()
  end.

title() -> "NXO: Secondary Auth".

body() ->
  Request = duoweb:sign_request(key(duo_ikey),
                                key(duo_skey),
                                key(duo_akey),
                                wf:to_list(wf:session(maybe_user))),
  Data = [{"host", key(duo_host)},
          {"sig-request", Request}],
  #iframe{data_fields=Data,
          id=duo_iframe,
          html_id=duo_iframe}.

verify_response() ->
  case duoweb:verify_response(key(duo_ikey),
                              key(duo_skey),
                              key(duo_akey),
                              wf:q(sig_response)) of
    [] ->
      wf:logout(),
      wf:redirect("/");
    Username ->
      wf:user(Username),
      wf:redirect("/")
  end.



key(Key) -> wf:to_list(nxo_settings:get(duo, Key)).
