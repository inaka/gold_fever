-module(gf_kathy_mon).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

%%% gen_server callbacks
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).
-export([start_link/1, process_name/1]).

-record(state, {kathy :: pid(), ref :: reference(), node :: node()}).
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
  binary_to_atom(<<"kathy-mon:", Bin/binary>>, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(node()) -> {ok, state(), 0}.
init(Node) -> {ok, #state{node = Node}, 0}.

-spec handle_info(_, state()) ->
  {noreply, state(), 100} | {stop, atom(), state()}.
handle_info(timeout, State = #state{kathy = undefined}) ->
  case erlang:whereis(gf_kathy:process_name(State#state.node)) of
    undefined ->
      _ = lager:notice("No pid yet for ~p", [State#state.node]),
      {noreply, State, 100};
    KathyPid ->
      MonRef = erlang:monitor(process, KathyPid),
      {noreply, State#state{kathy = KathyPid, ref = MonRef}}
  end;
handle_info(
  {'DOWN', Ref, process, Pid, Reason},
  State = #state{kathy = Pid, ref = Ref}) ->
  _ =
    lager:notice(
      "Kathy (~p) died (~p), and soon I will die too", [Pid, Reason]),
  {stop, Reason, State};
handle_info(Info, State) ->
  _ = lager:warning("Ignored info: ~p", [Info]),
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
