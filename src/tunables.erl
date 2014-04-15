%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午1:26
%%%-------------------------------------------------------------------
-module(tunables).
-author("Noisyfox").

-include("tunables.hrl").

%% API
-export([load_tunables/0]).


load_tunables() ->
  Default_tunables = #tunables{},
  IP_address = get_app_env(ip_address, Default_tunables#tunables.ip_address),
  Listen_port = get_app_env(listen_port, Default_tunables#tunables.listen_port),
  Ftp_banner = get_app_env(ftp_banner, Default_tunables#tunables.ftp_banner),
  Max_login_fails = get_app_env(max_login_fails, Default_tunables#tunables.max_login_fails),
  SSL_allowed = get_app_env(ssl_allowed, Default_tunables#tunables.ssl_allowed),
  SSL_cert = get_app_env(ssl_cert, Default_tunables#tunables.ssl_cert),
  SSL_key = get_app_env(ssl_key, Default_tunables#tunables.ssl_key),
  SSL_ca_cert = get_app_env(ssl_key, Default_tunables#tunables.ssl_ca_cert),
  PASV_enable = get_app_env(pasv_enable, Default_tunables#tunables.pasv_enable),
  #tunables{
    ip_address = IP_address,
    listen_port = Listen_port, ftp_banner = Ftp_banner, max_login_fails = Max_login_fails, pasv_enable = PASV_enable,
    ssl_allowed = SSL_allowed, ssl_cert = SSL_cert, ssl_key = SSL_key, ssl_ca_cert = SSL_ca_cert
  }.

get_app_env(Opt, Default) ->
  case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
      case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error -> Default
      end
  end.