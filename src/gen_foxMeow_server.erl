%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午2:55
%%%-------------------------------------------------------------------
-module(gen_foxMeow_server).
-author("Noisyfox").

%% API
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  % Path :: String
  % State Change :: {ok, State} OR {error, State}
  % File Name :: String
  % HelpInfo :: {Name, Description}
  [{init, 2}, % State, PropList (options) -> State
    {login, 3}, % State, Username, Password -> {true OR false, State}
    {current_directory, 1}, % State -> Path
    {make_directory, 2}, % State, Path -> State Change
    {change_directory, 2}, % State, Path -> State Change
    {list_files, 2}, % State, Path -> [FileInfo] OR {error, State}
    {remove_directory, 2}, % State, Path -> State Change
    {remove_file, 2}, % State, Path -> State Change
    {put_file, 4}, % State, File Name, (append OR write), Fun(Byte Count) -> State Change
    {get_file, 2}, % State, Path -> {ok, Fun(Byte Count)} OR error
    {file_info, 2}, % State, Path -> {ok, FileInfo} OR {error, ErrorCause}
    {rest, 2}, % State, Offset -> {ok, NewState} OR {error, ErrorCause}
    {rename_file, 3}, % State, From Path, To Path -> State Change
    {site_command, 3}, % State, Command Name String, Command Args String -> State Change
    {site_help, 1}, % State -> {ok, [HelpInfo]} OR {error, State}
    {disconnect, 1}]; % State -> State Change
behaviour_info(_) ->
  undefined.
