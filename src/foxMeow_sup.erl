%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午1:28
%%%-------------------------------------------------------------------
-module(foxMeow_sup).
-author("Noisyfox").

-behaviour(supervisor).

%% API
-export([start_link/3, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(HookModule::term(), LSock::term(), Tunables::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(HookModule, LSock, Tunables) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [HookModule, LSock, Tunables]).


start_child() ->
  supervisor:start_child(?SERVER, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([HookModule, LSock, Tunables]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 0,
  MaxSecondsBetweenRestarts = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = brutal_kill,
  Type = worker,

  Server = {'FoxMeow', {foxMeow_framework, start_link, [HookModule, LSock, Tunables]},
    Restart, Shutdown, Type, [foxMeow_framework]},

  {ok, {SupFlags, [Server]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
