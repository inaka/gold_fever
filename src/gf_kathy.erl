-module(gf_kathy).
-author('elbrujohalcon@inaka.net').

-behavior(gen_fsm).

-record(state, { node     :: node()
               , process  :: pid()
               , token    :: iodata()
               , server   :: atom()
               , answers  :: [atom()]
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

-spec server(node()) -> undefined | pid() | atom().
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
  {reply, State#state.server, StateName, State};
handle_sync_event(Msg, From, expecting_colors, State) ->
  expecting_colors(Msg, From, State).

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
  GenServer =
    case Name of
      Pid when is_pid(Pid) -> Pid;
      Name when is_atom(Name) -> {Name, State#state.node};
      _Other -> missing
    end,
  case GenServer of
    missing ->
      Message = io_lib:format(gold_fever:get_config(step3, missing), [name]),
      gf_node_monitor:send_message(State#state.node, Message),
      {next_state, expecting_maps, State};
    GenServer ->
      send_messages_to_gen_server(GenServer),
      Answers = gold_fever:get_config(step6, answers),
      NewState = State#state{server = Name, answers = Answers},
      {next_state, expecting_colors, NewState}
  end;
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
  gf_node_monitor:send_message(
    State#state.node, "This is not the way to talk to me"),
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
  Token = gf_real_kathy:token(State#state.node),
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
  {reply, term(), expecting_colors, state()} |
  {stop, normal, iodata(), state()}.
expecting_colors(
  #{token := Token, question := Q}, _From, State = #state{token = Token}) ->
  case {is_proper_question(Q), State#state.answers} of
    {true, []} ->
      Message = gold_fever:get_config(step6, message),
      {stop, normal, Message, State};
    {true, [Answer|Answers]} ->
      {reply, Answer, expecting_colors, State#state{answers = Answers}};
    {false, _} ->
      Message = gold_fever:get_config(step6, unknown),
      {reply, Message, expecting_colors, State}
  end;
expecting_colors(#{token := Token}, _From, State = #state{token = Token}) ->
  Message = io_lib:format(gold_fever:get_config(step6, missing), [question]),
  {reply, Message, expecting_colors, State};
expecting_colors(#{token := Wrong}, _From, State) ->
  Message = io_lib:format(gold_fever:get_config(step3, badauth), [Wrong]),
  {reply, Message, expecting_colors, State};
expecting_colors(#{}, _From, State) ->
  Message = io_lib:format(gold_fever:get_config(step3, badauth), [noone]),
  {reply, Message, expecting_colors, State};
expecting_colors(NotMap, _From, State) ->
  Message = io_lib:format(gold_fever:get_config(step3, unexpected), [NotMap]),
  {reply, Message, expecting_colors, State}.

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
  try gen_server:cast(GenServer, CastMsg)
  catch
    _:ECast -> lager:warning("~p couldn't get the cast: ~p", [GenServer, ECast])
  end,
  try GenServer ! InfoMsg
  catch
    _:EInfo -> lager:warning("~p couldn't get the cast: ~p", [GenServer, EInfo])
  end,
  try gen_server:call(GenServer, CallMsg) of
    R -> lager:notice("~p said ~p", [GenServer, R])
  catch
    _:ECall -> lager:warning("~p couldn't get the call: ~p", [GenServer, ECall])
  end.

is_proper_question(Question) when is_atom(Question) ->
  is_proper_question(atom_to_binary(Question, utf8));
is_proper_question(Question) ->
  Regexes = gold_fever:get_config(step6, regexes),
  not
    lists:member(
      nomatch, [re:run(Question, Regex, [caseless]) || Regex <- Regexes]).
