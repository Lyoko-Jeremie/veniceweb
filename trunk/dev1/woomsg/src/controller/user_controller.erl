-module(user_controller).
-export([handle_get/2, handle_post/2]).

%% 五种逻辑情况:
%% <1> 访问的当前用户不存在
%% <2> 用户没有登录, 访问页面
%% <3> 用户登录, 访问自己的页面
%% <4> 用户登录, 访问自己following的人的页面
%% <5> 用户登录, 访问陌生人的页面
%% 
%% CookieUsr是当前登录的用户, 
%% UrlUsername是当前访问的用户.  /user/Username
%% <1> 直接重定向到主页
%% <2> {logout, Username, undefined}
%% <3> {login_myself, Username, CookieUsr}
%% <4> {login_following, Username, CookieUsr}
%% <5> {login_no_following, Username, CookieUsr}
handle_get(Req, _DocRoot) ->
    UrlUsername = parse_username_from_url(Req),
    case woomsg_register:is_registered(UrlUsername) of
	true ->
	    case woomsg_login:auth(Req) of
	        {login, CookieUsr} ->
		    case UrlUsername =:= CookieUsr of
		        true ->
			    %% <3> 用户登录, 访问自己的页面
                            Data = user_view:index(login_myself, UrlUsername, CookieUsr),
                            Req:respond({200, [{"Content-Type","text/html"}], Data});
			false ->
			    case woomsg_following:is_following(CookieUsr, UrlUsername) of
			        true ->
				    %% <4> 用户登录, 访问自己following的人的页面
                                    Data = user_view:index(login_following, UrlUsername, CookieUsr),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data});
				false ->
				    %% <5> 用户登录, 访问陌生人页面
                                    Data = user_view:index(login_no_following, UrlUsername, CookieUsr),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data})
                            end
                    end;
		{logout, CookieUsr} ->
		    %% <2> 用户没有登录, 访问页面
                    Data = user_view:index(logout, UrlUsername, CookieUsr),
                    Req:respond({200, [{"Content-Type","text/html"}], Data})
            end;
	false ->
	    %% <1> 正在访问一个不存在的用户, 跳转到主页
            %% TODO: 单独的用户不存在页面
            Req:respond({302, [{"Location", "/"}], []})
    end.


handle_post(_Req, _DocRoot) ->
    ok.

%% 解析出URL中的用户名: Username
parse_username_from_url(Req) ->
    "/" ++ Path = Req:get(path),
    %% erlang:length("user/") = 5
    %% user/Username/xxx -> Username/xxx
    PathSuffix = lists:sublist(Path, 6, erlang:length(Path) - 5),
    
    %% Username/xxx -> Username
    %% Username     -> Username
    %%
    %% 注意: 
    %% Usename?key=val这种形式的URL将返回错误的用户名
    %% 我们在设计中避免使用这样的URL
    woomsg_util:list_index_prefix($/, PathSuffix).
