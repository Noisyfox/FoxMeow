%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2014 下午3:07
%%%-------------------------------------------------------------------
-author("Noisyfox").

-define(INFO(Msg),error_logger:info_msg(Msg)).
-define(INFO_F(Format,Msg),error_logger:info_msg(Format,Msg)).

-define(ERROR(Msg),error_logger:error_msg(Msg)).
-define(ERROR_F(Format,Msg),error_logger:error_msg(Format,Msg)).

-define(DEBUG(Msg),error_logger:info_msg(Msg)).
-define(DEBUG_F(Format,Msg),error_logger:info_msg(Format,Msg)).
