-module(gf_vault).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

%%% gen_server callbacks
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).
-export([start/1]).

-record(state, { callers :: #{pid() => unlocked | integer()}
               , config :: binary()
               }).
-type state() :: #state{}.

-spec start(map()) -> {ok, pid()}.
start(Config) ->
  gen_server:start({local, vault}, ?MODULE, Config, [{debug, [trace, log]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(map()) -> {ok, state()}.
init(Config) -> {ok, #state{config = term_to_binary(Config), callers = #{}}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(contents, {Caller, _Ref}, State) ->
  case maps:get(Caller, State#state.callers, notfound) of
    unlocked ->
      {reply, get_config(treasure, State), State};
    notfound ->
      NewCallers =
        maps:put(Caller, get_config(attempts, State), State#state.callers),
      {reply, get_config(warning, State), State#state{callers = NewCallers}};
    Attempts when Attempts < 0 ->
      {reply, get_config(rickroll, State), State};
    Attempts ->
      NewCallers = maps:put(Caller, Attempts - 1, State#state.callers),
      {reply, get_config(warning, State), State#state{callers = NewCallers}}
  end;
handle_call(Call, {Caller, Ref}, State) ->
  case maps:get(Caller, State#state.callers, notfound) of
    unlocked ->
      Message = get_config(unlocked, State),
      {reply, Message, State};
    notfound ->
      NewCallers =
        maps:put(Caller, get_config(attempts, State), State#state.callers),
      handle_call(Call, {Caller, Ref}, State#state{callers = NewCallers});
    0 ->
      Message = get_config(warning, State),
      NewCallers = maps:put(Caller, -1, State#state.callers),
      {reply, Message, State#state{callers = NewCallers}};
    Attempts when Attempts < 0 ->
      Message = get_config(unlocked, State),
      NewCallers = maps:put(Caller, Attempts - 1, State#state.callers),
      {reply, Message, State#state{callers = NewCallers}};
    Attempts ->
      Passwords = get_config(passwords, State),
      case lists:member(Call, Passwords) of
        true ->
          Message = get_config(unlocked, State),
          NewCallers = maps:put(Caller, unlocked, State#state.callers),
          {reply, Message, State#state{callers = NewCallers}};
        false ->
          Message =
            iolist_to_binary(
              io_lib:format(get_config(wrongpwd, State), [Attempts - 1])),
          NewCallers = maps:put(Caller, Attempts - 1, State#state.callers),
          {reply, Message, State#state{callers = NewCallers}}
      end
  end.

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

get_config(Key, State) -> maps:get(Key, binary_to_term(State#state.config)).
