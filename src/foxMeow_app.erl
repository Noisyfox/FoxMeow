%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午1:18
%%%-------------------------------------------------------------------
-module(foxMeow_app).
-author("Noisyfox").

-behaviour(application).

-include("foxMeow.hrl").

%% Application callbacks
-export([start/2,
  stop/1]).

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
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, [HookModule]) ->
  Tunables = tunables:load_tunables(),
  Opts = [list, {packet, 0}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}],
  {ok, LSock} = gen_tcp:listen(Tunables#tunables.listen_port, Opts),
  case foxMeow_sup:start_link(HookModule, LSock, Tunables) of
    {ok, Pid} ->
      foxMeow_sup:start_child(),
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
