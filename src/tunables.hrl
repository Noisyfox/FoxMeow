%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午1:24
%%%-------------------------------------------------------------------
-author("Noisyfox").

-record(tunables, {
  ip_address = undefined,
  listen_port = 21,
  ftp_banner = "(FoxMeow erlang Ftp server)",
  max_login_fails = 3,
  ssl_allowed = false,
  ssl_cert = undefined,
  ssl_key = undefined,
  ssl_ca_cert = undefined,
  pasv_enable = true,
  time_out = 60 * 1000
}).