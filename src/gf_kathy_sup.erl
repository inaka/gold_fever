-module(gf_kathy_sup).
-behavior(supervisor).

-export([start_link/0, start_child/1, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec start_child(node()) -> supervisor:startchild_ret().
start_child(Node) -> supervisor:start_child(?MODULE, [Node]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) ->
  {ok, {{simple_one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(noargs) ->
  KathyGroup =
    {gf_kathy_group, {gf_kathy_group, start_link, []},
      temporary, 1000, supervisor, [gf_kathy_group]},
  {ok, {{simple_one_for_one, 5, 10}, [KathyGroup]}}.
