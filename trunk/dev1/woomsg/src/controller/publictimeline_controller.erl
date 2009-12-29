-module(publictimeline_controller).
-export([handle_get/2, handle_post/2]).

handle_get(Req, _DocRoot) ->
    case woomsg_login:auth(Req) of
	{login, CookieUsr} ->
	    %% 用户登录
	    Data = publictimeline_view:index(login, CookieUsr),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data});
	{logout, CookieUsr} ->
	    %% 用户没有登录
	    Data = publictimeline_view:index(logout, CookieUsr),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data})
    end.

handle_post(_Req, _DocRoot) ->
    ok.

