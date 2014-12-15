-module(hooks).

-behaviour(application).
-behaviour(supervisor).

-export([
  install/3,
  uninstall/2,
  final/1,
  final/2,
  list/0,
  list/1,
  run/2,
  run/3,
  run/4,
  run_final/1,
  run_final/2,
  set/2,
  set/3,
  get/1,
  get/2,
  delete/1
  ]).

%% Application callbacks
-export([
  start/0,
  start/2,
  stop/1
  ]).

%% Supervisor callbacks
-export([
  start_link/0,
  init/1
  ]).

-include_lib("logger/include/log.hrl").

%%%===================================================================
%%% API
%%%===================================================================
final(Event) ->
  final(self(), Event).

final(Pid, Event) ->
  ets:insert(?MODULE,  {{final, Pid}, Event}),
  hooks_final:link(Pid).

install(_Event, _Weight, []) ->
  ok;
install(Event, Weight, [H | T]) ->
  install(Event, Weight, H),
  install(Event, Weight, T);
install(Event, Weight, HookFun) when is_integer(Weight) ->
  true = ets:insert(?MODULE, {{Event, Weight}, HookFun}),
  '_debug'("installed hook ~w function handler ~w with weight ~w", [Event, HookFun, Weight]),
  ok.

list() ->
  lists:flatten(ets:match(?MODULE, '$1')).

list(Event) ->
  lists:flatten(ets:match(?MODULE, {{Event, '_'}, '$1'})).

uninstall(final, Pid) ->
  ets:delete(?MODULE, {final, Pid}),
  ok;
uninstall(Event, HookFun) ->
  Weights = lists:flatten(ets:match(?MODULE, {{Event, '$1'}, HookFun})),
  lists:map(
    fun(X) ->
        true = ets:delete(?MODULE, {Event, X})
    end,
    Weights),
  ok.

run(Event, Params) ->
  run(self(), Event, Params, 5000).

run(Pid, Event, Params) when is_pid(Pid) ->
  run(Pid, Event, Params, 5000);
run(Event, Params, Timeout) ->
  run(self(), Event, Params, Timeout).

run(Pid, Event, Params, Timeout) when is_list(Params) ->
  '_debug'("running handlers for hook ~w with params ~w", [Event, Params]),
  Hooks = lists:flatten(ets:match(?MODULE, {{Event, '_'}, '$1'})),
  run_hooks(Hooks, [Pid | Params], [], Timeout).

run_final(Reason) ->
  run_final(self(), Reason).

run_final(Pid, Reason) ->
  case lists:flatten(ets:match(?MODULE, {{final, Pid}, '$1'})) of
    [] ->
      '_debug'("deleting options for ~w", [Pid]),
      hooks_options:delete(Pid),
      ok;
    [Event] ->
      hooks_finalize:run(Pid, Event, Reason)
  end.

set(Pid, Key, Value) when is_pid(Pid) ->
  hooks_options:set(Pid, Key, Value).

set(Key, Value) ->
  ?MODULE:set(self(), Key, Value).

get(Pid, Key) when is_pid(Pid) ->
  hooks_options:get(Pid, Key).

get(Key) ->
  ?MODULE:get(self(), Key).

delete(Key) ->
  hooks_options:delete(self(), Key).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {'_err'or, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start() ->
  application:start(?MODULE).

start(_StartType, _StartArgs) ->
  '_trace'("starting application"),
  ?MODULE:start_link().

start_link() ->
  '_trace'("starting supervisor"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  '_trace'("creating ets table"),
  ets:new(?MODULE, [ordered_set, public, named_table]),
  {
    ok,
    {
      {one_for_one, 5, 2000},
      [
        {
          hooks_options,
          {hooks_options, start_link, []},
          permanent,
          2000,
          worker,
          [hooks_options]
        },
        {
          hooks_final,
          {hooks_final, start_link, []},
          permanent,
          2000,
          supervisor,
          [hooks_final]
        },
        {
          hooks_finalize,
          {hooks_finalize, start_link, []},
          permanent,
          2000,
          supervisor,
          [hooks_finalize]
        }
      ]
    }
  }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
run_hooks([], _Params, Answers, _Timeout) ->
  lists:reverse(Answers);
run_hooks([{Module, Fun} | T], Params, Answers, Timeout) ->
  {Time, ModAnswer} = timer:tc(Module, Fun, Params ++ [Timeout]),
  '_debug'("~w:~w runtime ~wms", [Module, Fun, Time/1000]),
  case ModAnswer of
    stop            -> run_hooks([], Params, Answers, Timeout);
    {stop, Answer}  -> run_hooks([], Params, [Answer | Answers], Timeout);
    ok              -> run_hooks(T, Params, Answers, Timeout);
    {ok, Answer}    -> run_hooks(T, Params, [Answer | Answers], Timeout)
  end.
