%%% @doc main gold_fever supervisor
-module(gf_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the Supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init(noargs) ->
  {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(noargs) ->
  KatanaRandom =
    {ktn_random, {ktn_random, start_link, []},
      permanent, 5000, worker, [ktn_random]},
  NodeMonitor =
    {gf_node_monitor, {gf_node_monitor, start_link, []},
      permanent, 5000, worker, [gf_node_monitor]},
  KathySup =
    {gf_kathy_sup, {gf_kathy_sup, start_link, []},
      permanent, 5000, supervisor, [gf_kathy_sup]},
  TheRealKathy =
    {gf_real_kathy, {gf_real_kathy, start_link, []},
      permanent, 1000, worker, [gf_real_kathy]},
  { ok
  , { {one_for_one, 5, 10}
    , [KatanaRandom, NodeMonitor, KathySup, TheRealKathy]
    }
  }.
