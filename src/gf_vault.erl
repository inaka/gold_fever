-module(gf_vault).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

%%% gen_server callbacks
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).
-export([start/0]).

-record(state, {}).
-type state() :: #state{}.

-spec start() -> {ok, pid()}.
start() ->
  gen_server:start({local, vault}, ?MODULE, noargs, [{debug, [trace, log]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_call(X, term(), state()) -> {reply, {unknown, X}, state()}.
handle_call(X, _From, State) -> {reply, {unknown, X}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) -> {ok, state()}.
init(noargs) -> {ok, #state{}}.
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) -> {noreply, State}.
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
