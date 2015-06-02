-module(gf_node_monitor).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

%%% gen_server callbacks
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).
-export([start_link/0, send_message/2]).

-record(state, {}).
-type state() :: #state{}.

-type event() :: nodeup | nodedown.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(
    {local, ?MODULE}, ?MODULE, noargs, [{debug, [trace, log]}]).

-spec send_message(node(), iodata()) -> ok.
send_message(Node, Message) ->
  Rows = format(Message),
  lists:foreach(fun(Row) -> rpc:call(Node, erlang, display, [Row]) end, Rows).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) -> {ok, state()}.
init(noargs) ->
  ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
  {ok, #state{}}.

-spec handle_call(X, term(), state()) -> {reply, {unknown, X}, state()}.
handle_call(X, _From, State) -> {reply, {unknown, X}, State}.

-spec handle_info({event(), atom(), [tuple()]}, state()) -> {noreply, state()}.
handle_info({nodeup, Node, Data}, State) ->
  lager:notice("~p is UP! (~p)~n~n", [Node, Data]),
  ok = welcome_message(Node),
  gf_kathy:start(Node),
  {noreply, State};
handle_info({nodedown, Node, Data}, State) ->
  lager:notice("~p is DOWN! (~p).", [Node, Data]),
  gf_kathy:stop(Node),
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
welcome_message(Node) ->
  Message = gold_fever:get_config(step1, message),
  send_message(Node, Message).

format(Message) ->
  [lists:duplicate(80, $-)] ++
  [ binary_to_list(center(Bin))
  || Bin <- binary:split(
              iolist_to_binary(
                io_lib:format("~s~n", [Message])), <<"\n">>, [global, trim])
  ] ++
  [lists:duplicate(80, $-)].

center(<<Txt:78/binary>>) -> <<$|, Txt/binary, $|>>;
center(<<Txt:78/binary, Rest/binary>>) ->
  lager:warning("Too long message ~s || ~w", [Txt, Rest]),
  <<$|, Txt/binary, $|>>;
center(<<Txt:77/binary>>) -> <<$|, Txt/binary, " |">>;
center(Txt) -> center(<<$\s, Txt/binary, $\s>>).
