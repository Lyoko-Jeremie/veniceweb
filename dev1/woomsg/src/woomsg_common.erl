-module(woomsg_common).
-export([user_state/1]).

%% 通过检测Cookie中的数据, 判断用户的状态.
%%
%% 返回值
%% {login, Username}
%% {logout_remember, Username} 
%%   注意: Username有可能是undefined(当Cookie中没有用户名的时候) 
%% {logout_no_remember, undefined}
user_state(Req) ->
   case woomsg_login:auth(Req) of
       {login, CookieUsr} ->
	   %% 通过检测Cookie中的数据 -> 用户已经登录
           %% <1> 用户登录
           {login, CookieUsr};
       {logout, CookieUsr} ->
           %% 通过检测Cookie中的数据 -> 用户没有登录
           %% <2> Cookie中包含 remember=true
           %% <3> Cookie中不包含 remember=true
           case woomsg_login:remember(Req) of
	       true ->
		   {logout_remember, CookieUsr};
	       false ->
	           {logout_no_remember, undefined}
           end
   end.
