%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午2:53
%%%-------------------------------------------------------------------
-module(foxMeow_server).
-author("Noisyfox").

-behavior(gen_foxMeow_server).

-include("foxMeow.hrl").

-define(OPEN_FILE_OPTIONS, [read, raw, binary]).
-define(WRITE_FILE_OPTIONS, [write, raw, binary]).

%%gen_foxMeow_server callbacks.
-export([login/3,
  init/2,
  make_directory/2,
  current_directory/1,
  change_directory/2,
  list_files/2,
  remove_directory/2,
  remove_file/2,
  put_file/4,
  get_file/2,
  rest/2,
  file_info/2,
  rename_file/3,
  site_command/3,
  site_help/1,
  disconnect/1]).

%% API
-export([]).

init(InitialState, _) ->
  InitialState.

login(State, _Username, _Password) ->
%% 	Temp = {Username,Password},
%% 	Login = lists:member(Temp, Users),
%% 	?INFO_F("~p --- isLogin:~p ~n",[?MODULE,Login]),
  {true, State}.

current_directory(#state{conn = Conn}) ->
  Conn#connection_state.current_dir.

make_directory(#state{conn = Conn} = State, Directory) ->
  ClientDir = filename:join(Conn#connection_state.current_dir, Directory),
  Target = Conn#connection_state.root_dir ++ ClientDir,

  case file:make_dir(Target) of
    ok ->
      {ok, State#state{conn = #connection_state{current_dir = ClientDir}}};
    {error, _Reson} ->
      ?DEBUG_F("~p -- ~p Target error:~p ~n ", [?MODULE, Target, _Reson]),
      {error, eexist}
  end.

change_directory(#state{conn = Conn} = State, Directory) ->
  ClientDir = filename:join(Conn#connection_state.current_dir, Directory),
  Target = Conn#connection_state.root_dir ++ ClientDir,

  case filelib:is_file(Target) of
    true ->
      case filelib:is_dir(Target) of
        true ->
          {ok, State#state{conn = Conn#connection_state{current_dir = ClientDir}}};
        false ->
          io:format("error while changing directory ~p~n", [Target]),
          {error, State}
      end;
    false ->
      io:format("error while changing directory ~p~n", [Target]),
      {error, State}
  end.

disconnect(_) ->
  ok.

remove_file(#state{conn = Conn} = State, File) ->
  Target = [Conn#connection_state.root_dir, Conn#connection_state.current_dir, "/", File],
  Filename = filename:join(string:tokens(Target, "/")),

  ?INFO_F("~p --- Filename:~p ~n", [?MODULE, Filename]),
  case file:delete(Filename) of
    ok ->
      {ok, State};
    {error, Reason} ->
      ?DEBUG_F("~p --- remove_file,error:~p ~n", [?MODULE, Reason]),
      {error, Reason}
  end.

rename_file(#state{conn = Conn} = State, FromPath, ToPath) ->
  TargetFromPath = [Conn#connection_state.root_dir, Conn#connection_state.current_dir, "/", FromPath],
  FileFromPath = filename:join([TargetFromPath]),

  TargetToPath = [Conn#connection_state.root_dir, Conn#connection_state.current_dir, "/", ToPath],
  FileToPath = filename:join([TargetToPath]),

  case file:rename(FileFromPath, FileToPath) of
    ok ->
      {ok, State};
    {error, Reason} ->
      ?ERROR_F("~p --- rename_file,error:~p ~n", [?MODULE, Reason]),
      {error, rename_error}
  end.

remove_directory(#state{conn = Conn} = State, Directory) ->
  Target = [Conn#connection_state.root_dir, Conn#connection_state.current_dir, Directory],
  FilePath = filename:join([Target]),
  ?INFO_F("~p --- FilePath:~p ~n", [?MODULE, FilePath]),

  case file:del_dir(FilePath) of
    ok ->
      {ok, State};
    {error, Reason} ->
      ?DEBUG_F("~p --- remove_directory,error:~p ~n", [?MODULE, Reason]),
      {error, Reason}
  end.


list_files(State, "-al") ->
  {error, State};
list_files(#state{conn = Conn} = State, _Directory) ->
  Target = [Conn#connection_state.root_dir, Conn#connection_state.current_dir],
  FileDir = filename:join([Target]),

  case file:list_dir(FileDir) of
    {ok, FileList} ->
      FileInfoList = lists:foldl(fun(FileName, AccIn) ->
        FilePath = filename:join(FileDir, FileName),
        {ok, FileInfo} = file:read_file_info(FilePath),
        [{FileInfo, FileName} | AccIn]
      end, [], FileList),
      FileInfoList;
    {error, Reason} ->
      ?INFO_F("~p list_files,error:~p ~n", [?MODULE, Reason]),
      {error, State}
  end.

% mode could be append or write, but we're only supporting
% write.
% FileRetrievalFun is fun(ByteCount) and retrieves ByteCount bytes
%  and returns {ok, Bytes, Count} or done
put_file(#state{conn = Conn} = State, ProvidedFileName, _Mode, FileRetrievalFun) ->
  Target = [Conn#connection_state.root_dir, Conn#connection_state.current_dir, "/", ProvidedFileName],
  FilePath = filename:join([Target]),

  case prim_file:open(FilePath, ?WRITE_FILE_OPTIONS) of
    {ok, DataFD} ->
      {ok, _Count} = write_from_fun(DataFD, FileRetrievalFun),
      prim_file:close(DataFD),
      {ok, State};
    {error, Reason} ->
      ?INFO_F("~p -- put file error:~p ~n", [?MODULE, Reason]),
      {error, Reason}
  end.


get_file(#state{conn = #connection_state{offset = Offset} = Conn} = State, Path) ->
  Target = Conn#connection_state.root_dir ++ Conn#connection_state.current_dir ++ "/" ++ Path,
  FilePath = filename:join([Target]),

  {ok, DataFD} = prim_file:open(FilePath, ?OPEN_FILE_OPTIONS),
  {ok, reading_fun(State, DataFD, Offset)}.


rest(State, "0") ->
  {ok, State};
rest(#state{conn = Conn} = State, "100") ->
  {ok, State#state{conn = Conn#connection_state{offset = 0}}};
rest(#state{conn = Conn} = State, Arg) ->
  {Offset, _} = string:to_integer(Arg),
  {ok, State#state{conn = Conn#connection_state{offset = Offset}}}.

file_info(#state{conn = Conn}, File) ->
  Target = [Conn#connection_state.root_dir, Conn#connection_state.current_dir, File],
  FilePath = filename:join([Target]),
  file:read_file_info(FilePath).

site_command(_, _, _) ->
  {error, not_found}.

site_help(_) ->
  {error, not_found}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
write_from_fun(DataFD, Fun) ->
  write_from_fun(DataFD, 0, Fun).
write_from_fun(DataFD, Count, Fun) ->
  case Fun(1024) of
    {ok, Bytes, ReadCount} ->
      prim_file:write(DataFD, Bytes),
      write_from_fun(DataFD, Count + ReadCount, Fun);
    done ->
      io:format("DONE!"),
      {ok, Count}
  end.

reading_fun(State, DataFD, "0") ->
  reading_fun(State, DataFD);
reading_fun(State, DataFD, Offset) ->
  {ok, _} = prim_file:position(DataFD, Offset),
  reading_fun(State, DataFD).

reading_fun(State, DataFD) ->
  fun(ByteCount) ->
    case prim_file:read(DataFD, ByteCount) of
      {ok, Data} ->
        {ok, Data, reading_fun(State, DataFD)};
      eof ->
        prim_file:close(DataFD),
        {done, State}
    end
  end.
