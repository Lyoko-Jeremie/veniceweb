-module(upload_controller).
-export([handle_get/2, handle_post/2]).

handle_get(Req, _DocRoot) ->
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            Data = upload_view:index(login, Username),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
        {logout_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});
        {logout_remember, _Username} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});            
        {logout_no_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []})            
    end.


handle_post(Req, _DocRoot) ->
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            Res = woomsg_upload:parse_form(Req, true),
	    io:format("Upload-Res: ~p~n", [Res]),
            Data = upload_view:index(login, Username),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
        {logout_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});
        {logout_remember, _Username} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});            
        {logout_no_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []})            
    end.



