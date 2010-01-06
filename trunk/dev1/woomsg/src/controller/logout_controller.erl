-module(logout_controller).
-export([handle_get/1, handle_post/1]).

handle_get(Req) ->
    CookieUsr = woomsg_cookie:parse_cookie_username(Req),
    %% 用户注销, 重定向到主页.
    woomsg_session:set_logout(CookieUsr),
    Req:respond({302, [{"Location", "/"}], []}).

handle_post(Req) ->
    Req:respond({302, [{"Location", "/"}], []}).
