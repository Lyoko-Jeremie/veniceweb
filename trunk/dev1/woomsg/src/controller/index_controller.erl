-module(index_controller).
-export([handle_get/2, handle_post/2]).

-define(DEF_USERNAME, <<"请输入用户名">>).

handle_get(Req, _DocRoot) ->
    Data = case get_init(Req) of
	       {login, Username} ->
	           index_view:index(login, Username);
	       {rem_true, Username} ->
	           index_view:index(rem_true, Username);
	       {rem_false, Username} ->
	           index_view:index(rem_false, Username)
           end,
    Req:respond({200, [{"Content-Type","text/html"}], Data}).

handle_post(_Req, _DocRoot) ->
    ok.

%% 检测Cookie中的数据:
%% <1> 用户已经登录
%%     1.1 登录
%% <2> 用户没有登录
%%     2.1 Cookie中包含remember_true   
%%     2.2 Cookie中不包含remember_true
%%
%% 返回值
%% 1.1 {login, Username}
%% 2.1 {rem_true, Username}
%% 2.2 {rem_false, Username}
get_init(Req) ->
    case woomsg_login:auth(Req) of
        {login, CookieUsr} ->
	    %% 通过检测cookie中的数据 -> 用户已经登录
	    {login, CookieUsr};
	{logout, CookieUsr} ->
	    %% 通过检测cookie中的数据 -> 用户没有登录
	    %% <1> Cookie中包含remember=true
	    %% <2> Cookie中不包含remember=true
	    case woomsg_cookie:check_session_remember(Req) of
                true ->
		    Username = case CookieUsr of
		                   undefined ->
			               ?DEF_USERNAME;
			           _ ->
				       CookieUsr
                               end,
                    {rem_true, Username};   
		false ->
		    Username = ?DEF_USERNAME,
	            {rem_false, Username}
            end
    end.

    



