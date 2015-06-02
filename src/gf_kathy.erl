-module(gf_kathy).
-author('elbrujohalcon@inaka.net').

-behavior(gen_fsm).

-record(state, { node     :: node()
               , process  :: pid()
               , token    :: binary()
               , server   :: atom()
               }).
-type state() :: #state{}.

-export([start/1, stop/1, process_name/1, server/1]).
-export(
  [ start_link/1
  , init/1
  , handle_event/3
  , handle_sync_event/4
  , handle_info/3
  , terminate/3
  , code_change/4
  ]).
-export(
  [ expecting_flowers/3
  , expecting_flowers/2
  , expecting_maps/3
  , expecting_maps/2
  , expecting_colors/3
  , expecting_colors/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(node()) -> {ok, pid()}.
start(Node) -> gf_kathy_sup:start_child(Node).

-spec stop(node()) -> ok.
stop(Node) -> gf_kathy_group:stop(Node).

-spec process_name(node()) -> atom().
process_name(Node) ->
  Bin = atom_to_binary(Node, utf8),
  binary_to_atom(<<"kathy:", Bin/binary>>, utf8).

-spec server(node()) -> undefined | atom().
server(Node) ->
  gen_fsm:sync_send_all_state_event(process_name(Node), server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATELY EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(node()) -> {ok, pid()} | {error, term()}.
start_link(Node) ->
  Process = process_name(Node),
  gen_fsm:start_link({local, Process}, ?MODULE, Node, [{debug, [trace, log]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(node()) -> {ok, expecting_flowers, state(), 0}.
init(Node) -> {ok, expecting_flowers, #state{node = Node}, 0}.

-spec handle_event(stop, atom(), state()) -> {stop, normal, state()}.
handle_event(stop, _StateName, State) -> {stop, normal, State}.

-spec handle_sync_event(server, _From, atom(), state()) ->
  {reply, atom(), atom(), state()}.
handle_sync_event(server, _From, StateName, State) ->
  {reply, State#state.server, StateName, State}.

-spec handle_info(term(), atom(), state()) -> {next_state, atom(), state()}.
handle_info(flower, expecting_flowers, State) ->
  Message = gold_fever:get_config(step3, message),
  gf_node_monitor:send_message(State#state.node, Message),
  {next_state, expecting_maps, State};
handle_info(NotFlower, expecting_flowers, State) ->
  Message =
    io_lib:format(gold_fever:get_config(step2, unexpected), [NotFlower]),
  gf_node_monitor:send_message(State#state.node, Message),
  {next_state, expecting_flowers, State};
handle_info(
  #{token := Token, gen_server := Name}, expecting_maps,
  State = #state{token = Token}) ->
  handle_info(#{token => Token, name => Name}, expecting_maps, State);
handle_info(
  #{token := Token, name := {Name, Node}}, expecting_maps,
  State = #state{token = Token, node = Node}) ->
  handle_info(#{token => Token, name => Name}, expecting_maps, State);
handle_info(
  #{token := Token, name := Name}, expecting_maps,
  State = #state{token = Token}) ->
  send_messages_to_gen_server({Name, State#state.node}),
  {next_state, expecting_colors, State#state{server = Name}};
handle_info(#{token := Token}, expecting_maps, State = #state{token = Token}) ->
  Message = io_lib:format(gold_fever:get_config(step3, missing), [name]),
  gf_node_monitor:send_message(State#state.node, Message),
  {next_state, expecting_maps, State};
handle_info(#{token := Wrong}, expecting_maps, State) ->
  Message = io_lib:format(gold_fever:get_config(step3, badauth), [Wrong]),
  gf_node_monitor:send_message(State#state.node, Message),
  {next_state, expecting_maps, State};
handle_info(#{}, expecting_maps, State) ->
  Message = io_lib:format(gold_fever:get_config(step3, badauth), [noone]),
  gf_node_monitor:send_message(State#state.node, Message),
  {next_state, expecting_maps, State};
handle_info(NotMap, expecting_maps, State) ->
  Message = io_lib:format(gold_fever:get_config(step3, unexpected), [NotMap]),
  gf_node_monitor:send_message(State#state.node, Message),
  {next_state, expecting_maps, State};
handle_info(Info, StateName, State) ->
  lager:notice("~p received at ~p", [Info, StateName]),
  gf_node_monitor:send_message(State#state.node, "I'm not listening to you"),
  {next_state, StateName, State}.

-spec terminate(term(), atom(), state()) -> ok.
terminate(Reason, StateName, _State) ->
  lager:notice("Terminating in ~p with reason ~p", [StateName, Reason]).

-spec code_change(term() | {down, term()}, atom(), state(), term()) ->
    {ok, atom(), state()}.
code_change(_, StateName, State, _) -> {ok, StateName, State}.

-spec expecting_flowers(term(), _From, state()) ->
  {reply, not_an_fsm, expecting_flowers, state()}.
expecting_flowers(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, not_an_fsm, expecting_flowers, State}.

-spec expecting_flowers(term(), state()) ->
  {next_state, expecting_flowers, state()} |
  {next_state, atom(), state(), 1000}.
expecting_flowers(timeout, State) ->
  Token = ktn_random:generate(),
  Instructions = gold_fever:get_config(step2, message),
  Message = #{token => Token, kathy => self(), instructions => Instructions},
  {larry, State#state.node} ! Message,
  {next_state, expecting_flowers, State#state{token = Token}, 1000};
expecting_flowers(Request, State) ->
  lager:warning("Ignored Request: ~p", [Request]),
  {next_state, expecting_flowers, State}.

-spec expecting_maps(term(), _From, state()) ->
  {reply, not_an_fsm, expecting_maps, state()}.
expecting_maps(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, not_an_fsm, expecting_maps, State}.

-spec expecting_maps(term(), state()) ->
  {next_state, expecting_maps, state()}.
expecting_maps(Request, State) ->
  lager:warning("Ignored Request: ~p", [Request]),
  {next_state, expecting_maps, State}.

-spec expecting_colors(term(), _From, state()) ->
  {reply, not_an_fsm, expecting_colors, state()}.
expecting_colors(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, not_an_fsm, expecting_colors, State}.

-spec expecting_colors(term(), state()) ->
  {next_state, expecting_colors, state()}.
expecting_colors(Request, State) ->
  lager:warning("Ignored Request: ~p", [Request]),
  {next_state, expecting_colors, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_messages_to_gen_server(GenServer) ->
  ktn_task:wait_for_success(
    fun() ->
      {monitored_by, [_|_]} = erlang:process_info(self(), monitored_by)
    end),
  Message = gold_fever:get_config(step4, message),
  InfoMsg = [lists:nth(I, Message) || I <- lists:seq(1, length(Message), 3)],
  CastMsg = [lists:nth(I, Message) || I <- lists:seq(2, length(Message), 3)],
  CallMsg = [lists:nth(I, Message) || I <- lists:seq(3, length(Message), 3)],
  gen_server:cast(GenServer, CastMsg),
  GenServer ! InfoMsg,
  try gen_server:call(GenServer, CallMsg) of
    R -> lager:notice("~p said ~p", [GenServer, R])
  catch
    _:E -> lager:warning("~p couldn't get the call: ~p", [GenServer, E])
  end.
