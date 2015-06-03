-module(gf_real_kathy).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

%%% gen_server callbacks
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).
-export([start_link/0, token/1]).

-record(state, { tokens :: #{string() => node()}
               , callers :: [pid()]
               }).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(
    {local, kathy}, ?MODULE, noargs, [{debug, [trace, log]}]).

-spec token(node()) -> string().
token(Node) -> gen_server:call(kathy, {token, Node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) -> {ok, state()}.
init(noargs) ->
  {ok, #state{callers = [], tokens = #{}}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call({token, Node}, _From, State) ->
  Tokens = State#state.tokens,
  Token = ktn_random:generate(),
  {reply, Token, State#state{tokens = maps:put(Token, Node, Tokens)}};
handle_call(#{token := Token}, From, State) ->
  handle_call(Token, From, State);
handle_call(Token, {Caller, _}, State) ->
  {Welcome, NewState} =
    case lists:member(Caller, State#state.callers) of
      true -> {"Hey again!", State};
      false ->
        { gold_fever:get_config(step7, welcome)
        , State#state{callers = [Caller|State#state.callers]}
        }
    end,
  CallerNode = node(Caller),
  Message =
    case maps:get(Token, State#state.tokens, notfound) of
      CallerNode -> gold_fever:get_config(step7, message);
      notfound ->   gold_fever:get_config(step7, badauth);
      _OtherNode -> gold_fever:get_config(step7, expired)
    end,
  {reply, Welcome ++ " - " ++ Message, NewState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) -> {noreply, State}.
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
