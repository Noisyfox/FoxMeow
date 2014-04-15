%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午1:17
%%%-------------------------------------------------------------------
{application, foxMeow, [
  {description, "FoxMeow erlang Ftp server"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {foxMeow_app, [foxMeow_server]}},
  {env, []}
]}.