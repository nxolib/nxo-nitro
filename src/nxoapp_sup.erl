-module({{name}}_sup).

-behaviour(supervisor).
-include("nxo.hrl").
-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  Ret = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  nxo_db:apply_full_ddl(),
  Ret.


init([]) ->
  application:set_env(simple_bridge, document_root,
                      code:priv_dir({{name}}) ++ "/static"),

  nxo_db:start(),
  nxo_template:compile_all(),
  case nxo:is_development() of
    true -> sync:go();
    false -> ok
  end,

  lists:foreach(fun(App) -> ok = application:ensure_started(App) end,
                [ crypto
                , nitro_cache
                , nprocreg
                , simple_bridge
                , poolboy
                , epgsql
                , pgpool
                , bcrypt
                , erlpass
                ]),

  spawn(?MODULE, ping_db, []),
  nxo_template:compile_all(),

  %% initialize the caches
  nxo_template_name_cache:init(),

  %% manage event handlers
  start_event_handler(),

  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

start_event_handler() ->
  {ok, Pid} = gen_event:start_link(),
  nprocreg:register_pid(?EVENT, Pid).

ping_db() ->
  timer:sleep(10 * 60 * 1000),
  nxo_db:q(ping, []),
  ping_db().
