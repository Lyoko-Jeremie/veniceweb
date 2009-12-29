-module(upload_controller).
-export([handle_get/2, handle_post/2]).

handle_get(Req, _DocRoot) ->
    case woomsg_login:auth(Req) of
        {login, CookieUsr} ->
	    %% 用户登录
            Data = upload_view:index(CookieUsr),
            Req:respond({200, [{"Content-Type","text/html"}], Data});
	{logout, _CookieUsr} ->
	    %% 用户登录没登录, 重定向到主页
	    Req:respond({302, [{"Location", "/"}], []})
    end.


handle_post(_Req, _DocRoot) ->
    ok.



