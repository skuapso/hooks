-module(hooks_options).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  set/3,
  get/2,
  delete/1,
  delete/2
  ]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
  ]).

-record(state, {}).

-include_lib("logger/include/log.hrl").
%%%===================================================================
%%% API
%%%===================================================================
delete(Pid) ->
  trace("deleting hooks"),
  Opts = [Val || [Val] <- ets:match(?MODULE, {{Pid, '$1'}, '_'})],
  debug("found hooks: ~w", [Opts]),
  lists:map(
    fun (X) ->
        debug("deleting ~w", [{Pid, X}]),
        ets:delete(?MODULE, {Pid, X})
    end,
    Opts),
  ok.

delete(Pid, Key) ->
  ets:delete(?MODULE, {Pid, Key}),
  ok.

set(Pid, Key, Value) ->
  OldVal = ?MODULE:get(Pid, Key),
  ets:insert(?MODULE, {{Pid, Key}, Value}),
  hooks_final:link(Pid),
  OldVal.

get(Pid, Key) ->
  trace("getting ~w", [{Pid, Key}]),
  case ets:match(?MODULE, {{Pid, Key}, '$1'}) of
    [] ->
      undefined;
    [[Val]] ->
      Val
  end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  trace("init"),
  process_flag(trap_exit, true),
  ets:new(?MODULE, [public, set, named_table]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
  warning("unhandled call ~w from ~w", [Request, From]),
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
  warning("unhandled cast ~w", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
  warning("unhandled info ~w", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
