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
  {Welcome, NewCallers} =
    case lists:member(Caller, State#state.callers) of
      true -> {"Hey again!", State#state.callers};
      false ->
        {gold_fever:get_config(step7, welcome), [Caller|State#state.callers]}
    end,
  CallerNode = node(Caller),
  {Message, NewTokens} =
    case maps:get(Token, State#state.tokens, notfound) of
      Caller ->
        {gold_fever:get_config(step7, message), State#state.tokens};
      CallerNode ->
        { gold_fever:get_config(step7, message)
        , maps:put(Token, Caller, State#state.tokens)
        };
      notfound -> {gold_fever:get_config(step7, badauth), State#state.tokens};
      OtherPid when is_pid(OtherPid) ->
        case node(OtherPid) of
          CallerNode ->
            { gold_fever:get_config(step7, message)
            , maps:put(Token, Caller, State#state.tokens)
            };
          _OtherNode ->
            {gold_fever:get_config(step7, expired), State#state.tokens}
        end;
      _OtherNode -> {gold_fever:get_config(step7, expired), State#state.tokens}
    end,
  FinalMsg = Welcome ++ " - " ++ Message,
  {reply, FinalMsg, State#state{tokens = NewTokens, callers = NewCallers}}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(#{token := Token, address := Address} = Msg, State) ->
  case maps:get(Token, State#state.tokens, notfound) of
    Caller when is_pid(Caller) ->
      case http_uri:parse(to_str(Address)) of
        {error, Error} ->
          lager:warning("Can't parse url ~p: ~p", [Address, Error]),
          Caller ! gold_fever:get_config(step8, notaurl);
        {ok, ParsedUrl} ->
          put_image(ParsedUrl)
      end;
    notfound ->
      lager:warning("Unexpected token: ~p", [Msg]);
    Node ->
      gf_node_monitor:send_message(Node, gold_fever:get_config(step8, toosoon))
  end,
  {noreply, State};
handle_cast(#{token := Token} = Msg, State) ->
  case maps:get(Token, State#state.tokens, notfound) of
    Caller when is_pid(Caller) ->
      Message = io_lib:format(gold_fever:get_config(step8, missing), [address]),
      Caller ! iolist_to_binary(Message);
    notfound ->
      lager:warning("Unexpected token: ~p", [Msg]);
    Node ->
      gf_node_monitor:send_message(Node, gold_fever:get_config(step8, toosoon))
  end,
  {noreply, State};
handle_cast(_Cast, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) -> {noreply, State}.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
put_image({_Scheme, _UserInfo, Host, Port, [$/|Path], _Query}) ->
  ExpectedPath = gold_fever:get_config(step8, path),
  Headers = gold_fever:get_config(step8, headers),
  Filename = gold_fever:get_config(step8, image),
  {ok, Body} = file:read_file(Filename),
  Message = iolist_to_binary(gold_fever:get_config(step8, message)),
  FinalHeaders = Headers#{<<"x-instructions">> => Message},
  FinalPath =
    case lists:reverse(string:tokens(Path, [$/])) of
      [ExpectedPath|_] -> Path;
      _NotIncludingPocket -> string:join([Path, ExpectedPath], "/")
    end,
  {ok, Conn} = shotgun:open(Host, Port),
  try shotgun:put(Conn, FinalPath, FinalHeaders, Body, #{}) of
    {ok, Response} ->
      lager:notice(
        "Answer from ~s:~p~s: ~p~n", [Host, Port, FinalPath, Response]);
    {error, Error} ->
      lager:warning(
        "Error from ~s:~p~s: ~p~n", [Host, Port, FinalPath, Error])
  catch
    _:Exception ->
      lager:error("Exception from ~s:~p~s: ~p~n", [Host, Port, Path, Exception])
  after
    shotgun:close(Conn)
  end.

to_str(IOList) -> binary_to_list(iolist_to_binary(IOList)).
