-module(gf_kathy_mon_mon).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

%%% gen_server callbacks
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).
-export([start_link/1]).

-record(state, {kathy_mon :: pid(), ref :: reference(), node :: node()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(node()) -> {ok, pid()}.
start_link(Node) ->
  gen_server:start_link(
    {local, process_name(Node)}, ?MODULE, Node, [{debug, [trace, log]}]).

-spec process_name(node()) -> atom().
process_name(Node) ->
  Bin = atom_to_binary(Node, utf8),
  binary_to_atom(<<"kathy-mon-mon:", Bin/binary>>, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(node()) -> {ok, state(), 0}.
init(Node) -> {ok, #state{node = Node}, 0}.

-spec handle_info(_, state()) ->
  {noreply, state()} | {noreply, state(), 500} | {stop, atom(), state()}.
handle_info(timeout, State = #state{kathy_mon = undefined}) ->
  case erlang:whereis(gf_kathy_mon:process_name(State#state.node)) of
    undefined ->
      lager:notice("No pid yet for ~p", [State#state.node]),
      {noreply, State, 100};
    KathyMonPid ->
      MonRef = erlang:monitor(process, KathyMonPid),
      {noreply, State#state{kathy_mon = KathyMonPid, ref = MonRef}}
  end;
handle_info(timeout, State) ->
  Reason = gold_fever:get_config(step5, reason),
  kill(larry, Reason, State),
  case gf_kathy:server(State#state.node) of
    ServerPid when is_pid(ServerPid) ->
      lager:alert("Killing ~p", [ServerPid]), exit(ServerPid, Reason);
    ServerName ->
      kill(ServerName, Reason, State)
  end,
  {stop, normal, State};
handle_info(
  {'DOWN', Ref, process, _Pid, _Reason}, State = #state{ref = Ref}) ->
  Message = gold_fever:get_config(step5, message),
  gf_node_monitor:send_message(State#state.node, Message),
  {noreply, State, 500};
handle_info(Info, State) ->
  lager:warning("Ignored info: ~p", [Info]),
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_call(X, term(), state()) -> {reply, {unknown, X}, state()}.
handle_call(X, _From, State) -> {reply, {unknown, X}, State}.
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kill(Name, Reason, State) ->
  case rpc:call(State#state.node, erlang, whereis, [Name]) of
    undefined -> its_dead;
    Pid -> lager:alert("Killing ~p (~p)", [Name, Pid]), exit(Pid, Reason)
  end.
