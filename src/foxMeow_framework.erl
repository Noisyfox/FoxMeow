%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午1:39
%%%-------------------------------------------------------------------
-module(foxMeow_framework).
-author("Noisyfox").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("foxMeow.hrl").

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(HookModule :: term(), LSock :: term(), Tunables :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(HookModule, LSock, Tunables) ->
  gen_server:start_link(?MODULE, [HookModule, LSock, Tunables], []).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([HookModule, LSock, Tunables]) ->
  {ok, #state{module = HookModule, lsock = LSock, tunables = Tunables}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({cmd, {Command, Args} = Cmd}, #state{conn = Conn} = State) ->
  io:format("~p Get cmd: ~p~n", [self(), Cmd]),
  case handle_cmd(State, Command, Args) of
    {ok, NewState} ->
      grab_next_command(Conn, State#state.tunables#tunables.time_out),
      {noreply, NewState};
    {permission_deny} ->
      respond(Conn, ?FTP_NOPERM, "Permission denied."),
      grab_next_command(Conn, State#state.tunables#tunables.time_out),
      {noreply, State};
    _ ->
      {stop, normal, State}
  end;

handle_cast({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_cast({fake}, #state{conn = Conn} = State) ->
  grab_next_command(Conn, State#state.tunables#tunables.time_out),
  {noreply, State};

handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, #state{lsock = LSock, conn = undefined} = State) ->
  {ok, ClientSocket} = gen_tcp:accept(LSock),
  foxMeow_sup:start_child(),
  LocalAddr =
    case State#state.tunables#tunables.ip_address of
      undefined ->
        {ok, {Addr, _}} = inet:sockname(LSock),
        case Addr of
          {0, 0, 0, 0} -> {127, 0, 0, 1};
          _ -> Addr
        end;
      _ ->
        State#state.tunables#tunables.ip_address
    end,
  {ok, {IP, _Port}} = inet:peername(ClientSocket),
  inet:setopts(ClientSocket, [{active, false}]),
  Connect = #connection_state{sock_mode = gen_tcp, control_sock = ClientSocket, client_address = IP, server_address = LocalAddr},
  greeting(Connect, State#state.tunables#tunables.ftp_banner),
  gen_server:cast(self(), {fake}),
  {noreply, State#state{conn = Connect}};

handle_info(_Info, State) ->
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{conn = #connection_state{sock_mode = SocketMode, control_sock = Socket}}) ->
  io:format("~p Closed!~n", [self()]),
  (catch SocketMode:close(Socket)),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
response_code_string(110) -> "MARK yyyy = mmmm";
response_code_string(120) -> "Service ready in nnn minutes.";
response_code_string(125) -> "Data connection alredy open; transfere starting.";
response_code_string(150) -> "File status okay; about to open data connection.";
response_code_string(200) -> "Command okay.";
response_code_string(202) -> "Command not implemented, superfluous at this site.";
response_code_string(211) -> "System status, or system help reply.";
response_code_string(212) -> "Directory status.";
response_code_string(213) -> "File status.";
response_code_string(214) -> "Help message.";
response_code_string(215) -> "UNIX system type";
response_code_string(220) -> "Service ready for user.";
response_code_string(221) -> "Service closing control connection.";
response_code_string(225) -> "Data connection open; no transfere in progress";
response_code_string(226) -> "Closing data connection.";
response_code_string(227) -> "Entering Passive Mode (h1,h2,h3,h4,p1,p2).";
response_code_string(230) -> "User logged in, proceed.";
response_code_string(250) -> "Requested file action okay, completed.";
response_code_string(257) -> "PATHNAME created.";
response_code_string(331) -> "User name okay, need password.";
response_code_string(332) -> "Need account for login.";
response_code_string(350) -> "Requested file action pending further information.";
response_code_string(421) -> "Service not available, closing control connection.";
response_code_string(425) -> "Can't open data connection.";
response_code_string(426) -> "Connection closed; transfere aborted.";
response_code_string(450) -> "Requested file action not taken.";
response_code_string(451) -> "Requested action not taken: local error in processing.";
response_code_string(452) -> "Requested action not taken.";
response_code_string(500) -> "Syntax error, command unrecognized.";
response_code_string(501) -> "Syntax error in parameters or arguments.";
response_code_string(502) -> "Command not implemented.";
response_code_string(503) -> "Bad sequence of commands.";
response_code_string(504) -> "Command not implemented for that parameter.";
response_code_string(530) -> "Not logged in.";
response_code_string(532) -> "Need account for storing files.";
response_code_string(550) -> "Requested action not taken.";
response_code_string(551) -> "Requested action aborted: page type unkown.";
response_code_string(552) -> "Requested file action aborted.";
response_code_string(553) -> "Requested action not taken.";
response_code_string(_) -> "N/A".

respond(Conn, ResponseCode) ->
  respond(Conn, ResponseCode, response_code_string(ResponseCode)).

respond(#connection_state{sock_mode = SocketMode, control_sock = Socket, encode = Encode}, ResponseCode, Message) ->
  Line = integer_to_list(ResponseCode) ++ " " ++ Message ++ "\r\n",
  SocketMode:send(Socket, encode_string(Line, Encode)).

respond_raw(#connection_state{sock_mode = SocketMode, control_sock = Socket, encode = Encode}, Line) ->
  SocketMode:send(Socket, encode_string(Line ++ "\r\n", Encode)).

strip_newlines(S) ->
  lists:foldr(fun(C, A) ->
    string:strip(A, right, C) end,
    S,
    "\r\n").

parse_input(Input, Encode) ->
  Tokens = string:tokens(Input, " "),
  [Command | Args] = lists:map(fun(S) -> strip_newlines(S) end,
    Tokens),
  {list_to_atom(string:to_lower(Command)), decode_string(list_to_binary(string:join(Args, " ")), Encode)}.

grab_next_command(#connection_state{sock_mode = SocketMode, control_sock = Socket, encode = Encode}, Timeout) ->
  case SocketMode:recv(Socket, 0, Timeout) of
    {ok, Input} ->
      Command = parse_input(Input, Encode),
      gen_server:cast(self(), {cmd, Command});
    {error, _Reason} ->
      gen_server:cast(self(), {tcp_closed, Socket})
  end.

greeting(Conn, Banner) ->
  respond(Conn, ?FTP_GREET, Banner).

ssl_options(State) ->
  [{keyfile, State#state.tunables#tunables.ssl_key},
    {certfile, State#state.tunables#tunables.ssl_cert},
    {cacertfile, State#state.tunables#tunables.ssl_ca_cert}].

% parse address on form:
% d1,d2,d3,d4,p1,p2  => { {d1,d2,d3,d4}, port} -- ipv4
% h1,h2,...,h32,p1,p2 => {{n1,n2,..,n8}, port} -- ipv6
% Taken from jungerl/ftpd
parse_address(Str) ->
  paddr(Str, 0, []).

paddr([X | Xs], N, Acc) when X >= $0, X =< $9 -> paddr(Xs, N * 10 + (X - $0), Acc);
paddr([X | Xs], _N, Acc) when X >= $A, X =< $F -> paddr(Xs, (X - $A) + 10, Acc);
paddr([X | Xs], _N, Acc) when X >= $a, X =< $f -> paddr(Xs, (X - $a) + 10, Acc);
paddr([$,, $, | _Xs], _N, _Acc) -> error;
paddr([$, | Xs], N, Acc) -> paddr(Xs, 0, [N | Acc]);
paddr([], P2, [P1, D4, D3, D2, D1]) -> {ok, {{D1, D2, D3, D4}, P1 * 256 + P2}};
paddr([], P2, [P1 | As]) when length(As) == 32 ->
  case addr6(As, []) of
    {ok, Addr} -> {ok, {Addr, P1 * 256 + P2}};
    error -> error
  end;
paddr(_, _, _) -> error.

addr6([H4, H3, H2, H1 | Addr], Acc) when H4 < 16, H3 < 16, H2 < 16, H1 < 16 ->
  addr6(Addr, [H4 + H3 * 16 + H2 * 256 + H1 * 4096 | Acc]);
addr6([], Acc) -> {ok, list_to_tuple(Acc)};
addr6(_, _) -> error.

format_port(PortNumber) ->
  [A, B] = binary_to_list(<<PortNumber:16>>),
  {A, B}.

clean_connection(#state{conn = Conn} = State) ->
  State#state{conn = Conn#connection_state{pasv_connection = undefined, port_connection = undefined}}.

data_connection(Conn, State) ->
  respond(Conn, 150),
  case establish_data_connection(Conn) of
    {ok, DataSocket} ->
      case Conn#connection_state.protection_mode of
        clear ->
          {gen_tcp, DataSocket};
        private ->
          case ssl:ssl_accept(DataSocket,
            ssl_options(State)) of
            {ok, SslSocket} ->
              {ssl, SslSocket};
            E ->
              respond(Conn, 425),
              throw({error, E})
          end
      end;
    {error, {msg, Msg}} = Error ->
      respond(Conn, ?FTP_BADSENDCONN, Msg),
      throw(Error);
    {error, _} = Error ->
      respond(Conn, ?FTP_BADSENDCONN),
      throw(Error)
  end.

% passive -- accepts an inbound connection
establish_data_connection(#connection_state{pasv_connection = {pasv, Listen, _}}) ->
  Socket = gen_tcp:accept(Listen),
  inet:close(Listen),
  Socket;

% active -- establishes an outbound connection
establish_data_connection(#connection_state{port_connection = {port, Addr, Port}, data_mode = Mode}) ->
  gen_tcp:connect(Addr, Port, [{active, false}, Mode]);

establish_data_connection(#connection_state{pasv_connection = undefined, port_connection = undefined}) ->
  {error, {msg, "Use PORT or PASV first."}}.

% Taken from jungerl/ftpd

file_info_to_string(Info) ->
  format_type(Info#file_info.type) ++
    format_access(Info#file_info.mode) ++ " " ++
    format_number(type_num(Info#file_info.type), 2, $ ) ++ " " ++
    format_number(Info#file_info.uid, 5, $ ) ++ " " ++
    format_number(Info#file_info.gid, 5, $ ) ++ " " ++
    format_number(Info#file_info.size, 8, $ ) ++ " " ++
    format_date(Info#file_info.mtime).

format_mdtm_date({{Year, Month, Day}, {Hours, Mins, Secs}}) ->
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
    [Year, Month, Day, Hours, Mins, erlang:trunc(Secs)])).

format_date({Date, Time}) ->
  {Year, Month, Day} = Date,
  {Hours, Min, _} = Time,
  {LDate, _LTime} = calendar:local_time(),
  {LYear, _, _} = LDate,
  format_month_day(Month, Day) ++
  if LYear > Year ->
    format_year(Year);
    true ->
      format_time(Hours, Min)
  end.

format_month_day(Month, Day) ->
  io_lib:format("~s ~2.2w", [month(Month), Day]).

format_year(Year) ->
  io_lib:format(" ~5.5w", [Year]).

format_time(Hours, Min) ->
  io_lib:format(" ~2.2.0w:~2.2.0w", [Hours, Min]).

format_type(regular) -> "-";
format_type(directory) -> "d";
format_type(_) -> "?".

type_num(regular) -> 1;
type_num(directory) -> 4;
type_num(_) -> 0.

format_access(Mode) ->
  format_rwx(Mode bsr 6) ++ format_rwx(Mode bsr 3) ++ format_rwx(Mode).

format_rwx(Mode) ->
  [if Mode band 4 == 0 -> $-; true -> $r end,
    if Mode band 2 == 0 -> $-; true -> $w end,
    if Mode band 1 == 0 -> $-; true -> $x end].

format_number(X, N, LeftPad) when X >= 0 ->
  Ls = integer_to_list(X),
  Len = length(Ls),
  if Len >= N -> Ls;
    true ->
      lists:duplicate(N - Len, LeftPad) ++ Ls
  end.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

list_files_to_socket(DataSocket, Encode, Files) ->
  lists:map(fun({Info, Name}) ->
    bf_send(DataSocket, encode_string(lists:flatten(
      file_info_to_string(Info) ++ " " ++ Name ++ "\r\n"), Encode)) end,
    Files),
  ok.

list_file_names_to_socket(DataSocket, Encode, Files) ->
  lists:map(fun({_, Name}) ->
    bf_send(DataSocket, encode_string(lists:flatten(Name ++ "\r\n"), Encode)) end,
    Files),
  ok.

bf_send({SockMod, Socket}, Data) ->
  SockMod:send(Socket, Data).

bf_close({SockMod, Socket}) ->
  SockMod:close(Socket).

bf_recv({SockMod, Socket}) ->
  SockMod:recv(Socket, 0).

encode_string(Str, utf8) ->
  unicode:characters_to_binary(Str, unicode, utf8);
encode_string(Str, ansi) ->
  unicode:characters_to_binary(Str, unicode, utf8).

decode_string(Bin, utf8) ->
  [Str] = io_lib:format("~ts", [Bin]),
  Str.

write_fun(Socket, Fun) ->
  case Fun(?FILE_PRE_READ_TOM(1)) of
    {ok, Bytes, NextFun} ->
      bf_send(Socket, Bytes),
      write_fun(Socket, NextFun);
    {done, NewState} ->
      {ok, NewState}
  end.

%% FTP COMMANDS
handle_cmd(#state{conn = Conn, module = Module} = State, Command, Arg) ->
  handle_cmd(Module, Conn, State, Command, Arg).

handle_cmd(_, _, State, '', _) ->
  {ok, State};

handle_cmd(Module, Conn, State, quit, _) ->
  respond(Conn, ?FTP_GOODBYE, "Goodbye."),
  Module:disconnect(State),
  quit;

handle_cmd(_, Conn, State, noop, _) ->
  respond(Conn, ?FTP_NOOPOK, "NOOP ok."),
  {ok, State};

handle_cmd(_, Conn, State, user, Arg) ->
  if
    Conn#connection_state.user =/= undefined ->
      if
        Arg =:= Conn#connection_state.user ->
          respond(Conn, ?FTP_GIVEPWORD, "Any password will do.");
        true ->
          respond(Conn, ?FTP_LOGINERR, "Can't switch to another user!")
      end,
      {ok, State};
    true ->
      respond(Conn, ?FTP_GIVEPWORD),
      {ok, State#state{conn = Conn#connection_state{user = Arg}}}
  end;

handle_cmd(Module, Conn, State, pass, Arg) ->
  if
    Conn#connection_state.login_fails > State#state.tunables#tunables.max_login_fails ->
      respond(Conn, ?FTP_LOGINERR),
      close;
    Conn#connection_state.user =:= undefined ->
      respond(Conn, ?FTP_NEEDUSER, "Login with USER first."),
      {ok, State};
    Conn#connection_state.authenticated_state =:= authenticated ->
      respond(Conn, ?FTP_LOGINOK, "Already logged in."),
      {ok, State};
    true ->
      case Module:login(State, Conn#connection_state.user, Arg) of
        {true, NewState} ->
          respond(Conn, ?FTP_LOGINOK),
          {ok, NewState#state{conn = Conn#connection_state{authenticated_state = authenticated}}};
        _ ->
          respond(Conn, ?FTP_LOGINERR),
          {ok, State#state{conn = Conn#connection_state.login_fails + 1}}
      end
  end;

handle_cmd(_, #connection_state{control_sock = Socket} = Conn, State, auth, Arg) ->
  if
    State#state.tunables#tunables.ssl_allowed =:= false ->
      {permission_deny};
    true ->
      case string:to_lower(Arg) of
        "tls" ->
          case ssl:ssl_accept(Socket, ssl_options(State)) of
            {ok, SSLSocket} ->
              respond(Conn, ?FTP_AUTHOK, "Auth okay."),
              {ok, State#state{conn = Conn#connection_state{sock_mode = ssl, control_sock = SSLSocket}}};
            _ ->
              respond(Conn, ?FTP_BADAUTH, "Auth faild."),
              {ok, State}
          end;
        _ ->
          respond(Conn, ?FTP_BADAUTH, "Unsupported security extension."),
          {ok, State}
      end
  end;

%% ^^^ from this point down every command requires authentication ^^^
handle_cmd(_, _, #state{conn = #connection_state{authenticated_state = unauthenticated}}, _, _) ->
  {permission_deny};

handle_cmd(_, Conn, State, syst, _) ->
  respond(Conn, ?FTP_SYSTOK, "UNIX Type: L8"),
  {ok, State};

handle_cmd(_, Conn, State, feat, _) ->
  respond_raw(Conn, integer_to_list(?FTP_FEAT) ++ "-Features:"),
  respond_raw(Conn, " MDTM"),
  if
    State#state.tunables#tunables.pasv_enable ->
      respond_raw(Conn, " PASV");
    true -> void
  end,
  respond_raw(Conn, " REST STREAM"),
  respond_raw(Conn, " SIZE"),
  respond_raw(Conn, " UTF8"),
  respond(Conn, ?FTP_FEAT, "End"),
  {ok, State};

handle_cmd(Module, Conn, State, pwd, _) ->
  respond(Conn, ?FTP_PWDOK, "\"" ++ Module:current_directory(State) ++ "\""),
  {ok, State};

handle_cmd(Module, Conn, State, cdup, _) ->
  handle_cmd(Module, Conn, State, cwd, "..");

handle_cmd(Module, Conn, State, cwd, Arg) ->
  case Module:change_directory(State, Arg) of
    {ok, NewState} ->
      respond(Conn, ?FTP_CWDOK, "directory changed to \"" ++ Module:current_directory(NewState) ++ "\""),
      {ok, NewState};
    {error, _} ->
      respond(Conn, ?FTP_FILEFAIL, "Unable to change directory"),
      {ok, State}
  end;

handle_cmd(_, Conn, State, type, Arg) ->
  case Arg of
    "I" ->
      respond(Conn, ?FTP_TYPEOK, "Switching to Binary mode."),
      {ok, State#state{conn = Conn#connection_state{data_mode = binary}}};
    "A" ->
      respond(Conn, ?FTP_TYPEOK, "Switching to ASCII mode."),
      {ok, State#state{conn = Conn#connection_state{data_mode = list}}};
    _ ->
      respond(Conn, ?FTP_BADOPTS, "Only TYPE I or TYPE A may be used."),
      {ok, State}
  end;

handle_cmd(Module, Conn, State, pasv, Arg) ->
  case Conn#connection_state.pasv_connection of
    {pasv, PasvSock, _} ->
      gen_tcp:close(PasvSock),
      NewConn = Conn#connection_state{pasv_connection = undefined},
      handle_cmd(Module, NewConn, State#state{conn = NewConn}, pasv, Arg);
    undefined ->
      case gen_tcp:listen(0, [{active, false}, Conn#connection_state.data_mode]) of
        {ok, Listen} ->
          {ok, {_, Port}} = inet:sockname(Listen),
          {S1, S2, S3, S4} = IP = Conn#connection_state.server_address,
          {P1, P2} = format_port(Port),
          PasvConn = {pasv, Listen, {IP, Port}},
          respond(Conn,
            ?FTP_PASVOK,
            lists:flatten(
              io_lib:format("Entering Passive Mode (~p,~p,~p,~p,~p,~p)",
                [S1, S2, S3, S4, P1, P2]))),
          {ok, State#state{conn = Conn#connection_state{pasv_connection = PasvConn}}};
        {error, _} ->
          respond(Conn, ?FTP_BADSENDCONN),
          {ok, State}
      end
  end;

handle_cmd(_, Conn, State, port, Arg) ->
  case parse_address(Arg) of
    {ok, {Addr, Port}} ->
      respond(Conn, ?FTP_PORTOK),
      {ok, State#state{conn = Conn#connection_state{port_connection = {port, Addr, Port}}}};
    _ ->
      respond(Conn, 452, "Error parsing address."),
      {ok, State}
  end;

handle_cmd(Module, Conn, State, list, Arg) ->
  case Module:list_files(State, Arg) of
    {error, _} ->
      respond(Conn, ?FTP_FILEFAIL),
      {ok, State};
    Files ->
      DataSocket = data_connection(Conn, State),
      list_files_to_socket(DataSocket, Conn#connection_state.encode, Files),
      bf_close(DataSocket),
      respond(Conn, ?FTP_TRANSFEROK),
      {ok, clean_connection(State)}
  end;

handle_cmd(Module, Conn, State, nlst, Arg) ->
  case Module:list_files(State, Arg) of
    {error, _} ->
      respond(Conn, ?FTP_FILEFAIL),
      {ok, State};
    Files ->
      DataSocket = data_connection(Conn, State),
      list_file_names_to_socket(DataSocket, Conn#connection_state.encode, Files),
      bf_close(DataSocket),
      respond(Conn, ?FTP_TRANSFEROK),
      {ok, clean_connection(State)}
  end;

handle_cmd(Module, Conn, State, mdtm, Arg) ->
  case Module:file_info(State, Arg) of
    {ok, FileInfo} ->
      respond(Conn, ?FTP_MDTMOK, format_mdtm_date(FileInfo#file_info.mtime));
    _ ->
      respond(Conn, ?FTP_FILEFAIL)
  end,
  {ok, State};

handle_cmd(Module, Conn, State, size, Arg) ->
  case Module:file_info(State, Arg) of
    {ok, FileInfo} ->
      respond(Conn, ?FTP_SIZEOK, integer_to_list(FileInfo#file_info.size));
    _ ->
      respond(Conn, ?FTP_FILEFAIL)
  end,
  {ok, State};

handle_cmd(Module, Conn, State, retr, Arg) ->
  try
    case Module:get_file(State, Arg) of
      {ok, Fun} ->
        DataSocket = data_connection(Conn, State),
        {ok, NewState} = write_fun(DataSocket, Fun),
        bf_close(DataSocket),
        respond(Conn, ?FTP_TRANSFEROK),
        {ok, clean_connection(NewState)};
      error ->
        respond(Conn, ?FTP_FILEFAIL),
        {ok, State}
    end
  catch
    _ ->
      respond(Conn, ?FTP_FILEFAIL),
      {ok, State}
  end;

handle_cmd(_, Conn, State, _, _) ->
  respond(Conn, ?FTP_BADCMD),
  {ok, State}.
