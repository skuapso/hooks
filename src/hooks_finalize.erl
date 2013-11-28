-module(hooks_finalize).

-behaviour(gen_server).

%% API
-export([start_link/0, run/3]).

%% gen_server callbacks
-export([init/1,
         init/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-include_lib("logger/include/log.hrl").
%%%===================================================================
%%% API
%%%===================================================================
run(Pid, Event, Reason) ->
  gen_server:cast(?MODULE, {finalize, Pid, Event, Reason}).

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
  process_flag(trap_exit, true),
  ets:new(?MODULE, [public, set, named_table]),
  {ok, #state{}}.

init(Pid, Event, Reason) ->
  trace("init"),
  proc_lib:init_ack({ok, self()}),
  trace("running hook"),
  hooks:run(Pid, Event, [Reason], infinity),
  trace("exiting"),
  ok.

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
handle_call(_Request, _From, State) ->
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
handle_cast({finalize, Pid, Event, Reason}, State) ->
  trace("starting finalize for ~w (~w)", [Pid, Event]),
  case ets:match(?MODULE, {'$1', {Pid, '_', '_'}}) of
    [] ->
      {ok, FPid} = start_link(Pid, Event, Reason),
      ets:insert(?MODULE, {FPid, {Pid, Event, Reason}});
    _Else -> ok
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
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
handle_info({'EXIT', FPid, normal}, State) ->
  trace("~w finilized", [FPid]),
  [[Pid]] = ets:match(?MODULE, {FPid, {'$1', '_', '_'}}),
  ets:delete(?MODULE, FPid),
  hooks:uninstall(final, Pid),
  hooks:run_final(Pid, normal),
  {noreply, State};
handle_info({'EXIT', FPid, FReason}, State) ->
  [[Pid, Event, Reason]] = ets:match(?MODULE, {FPid, {'$1', '$2', '$3'}}),
  warning("~w finalization failed: ~w", [Pid, FReason]),
  ets:delete(?MODULE, FPid),
  run(Pid, Event, Reason),
  {noreply, State};
handle_info(Info, State) ->
  warning("unhandled info msg ~w", [Info]),
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
start_link(Pid, Event, Reason) ->
  proc_lib:start_link(?MODULE, init, [Pid, Event, Reason]).
