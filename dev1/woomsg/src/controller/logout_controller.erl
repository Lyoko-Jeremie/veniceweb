-module(logout_controller).
-export([handle_get/2, handle_post/2]).

handle_get(Req, _DocRoot) ->
    CookieUsr = woomsg_cookie:parse_cookie_username(Req),
    %% 用户注销, 重定向到主页.
    woomsg_session:set_logout(CookieUsr),
    Req:respond({302, [{"Location", "/"}], []}).

handle_post(Req, _DocRoot) ->
    Req:respond({302, [{"Location", "/"}], []}).
