-module(setting_controller).
-export([handle_get/2, handle_post/2]).

handle_get(Req, _DocRoot) ->
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            setting_view:index(login, Username);
        {logout_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});
        {logout_remember, Username} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});            
        {logout_no_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []})            
    end.


handle_post(_Req, _DocRoot) ->
    ok.

