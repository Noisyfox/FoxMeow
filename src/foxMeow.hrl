%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午1:22
%%%-------------------------------------------------------------------
-author("Noisyfox").

-include_lib("kernel/include/file.hrl").
-include("tunables.hrl").
-include("ftp_code.hrl").
-include("log.hrl").

-record(connection_state,
{
  authenticated_state = unauthenticated,
  user = undefined,
  login_fails = 0,
  protection_mode = clear,
  sock_mode = gen_tcp,
  control_sock = undefined,
  server_address = undefined,
  client_address = undefined,
  root_dir = "D:/",
  current_dir = "/",
  offset = 0,
  pasv_connection = undefined,
  port_connection = undefined,
  data_mode = binary,
  encode = utf8
}).

-record(state,
{
  module,
  lsock,
  tunables,
  conn
}).
