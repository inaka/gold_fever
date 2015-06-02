-module(gf_kathy_group).
-behavior(supervisor).

-export([start_link/1, stop/1, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(node()) -> {ok, pid()}.
start_link(Node) ->
  {ok, P} = supervisor:start_link({local, process_name(Node)}, ?MODULE, Node),
  sys:trace(P, true),
  {ok, P}.

-spec stop(node()) -> ok.
stop(Node) ->
  case whereis(process_name(Node)) of
    undefined -> ok;
    Pid -> exit(Pid, kill)
  end.

-spec process_name(node()) -> atom().
process_name(Node) ->
  Bin = atom_to_binary(Node, utf8),
  binary_to_atom(<<"kathy-group:", Bin/binary>>, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(node()) ->
  {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(Node) ->
  Kathy =
    {gf_kathy, {gf_kathy, start_link, [Node]},
      temporary, brutal_kill, worker, [gf_kathy]},
  KathyMon =
    {gf_kathy_mon, {gf_kathy_mon, start_link, [Node]},
      temporary, brutal_kill, worker, [gf_kathy_mon]},
  KathyMonMon =
    {gf_kathy_mon_mon, {gf_kathy_mon_mon, start_link, [Node]},
      temporary, brutal_kill, worker, [gf_kathy_mon_mon]},
  {ok, {{one_for_one, 5, 10}, [Kathy, KathyMon, KathyMonMon]}}.
