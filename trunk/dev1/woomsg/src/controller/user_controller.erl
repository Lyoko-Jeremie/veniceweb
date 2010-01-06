-module(user_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).

%% 五种逻辑情况:
%% <1> 访问的当前用户不存在
%% <2> 用户登录, 访问自己的页面
%%     用户登录, 访问自己following的人的页面
%%     用户登录, 访问陌生人的页面
%% <3> 用户没登录
%%
%% UrlUsername是当前访问的用户.  /user/Username
%%
%% 传递给view的参数:
%% {login, Username, ext_myself, UrlUsername}
%% {login, Username, ext_following, UrlUsername}
%% {login, Username, ext_no_following, UrlUsername}
%% {logout_remember, ?DEF_USERNAME, undefined, UrlUsername}
%% {logout_remember, Username, undefined, UrlUsername}
%% {logout_no_remember, ?DEF_USERNAME, undefined, UrlUsername}
handle_get(Req) ->
    UrlUsername = parse_username_from_url(Req),
    case woomsg_register:is_registered(UrlUsername) of
	true ->
            case woomsg_common:user_state(Req) of
                {login, Username} ->
	            %% <2> 用户登录
                    case Username =:= UrlUsername of
		        true ->
			%% 2.1 用户登录, 访问自己的页面
		            Data = user_view:index(login, Username, ext_myself, UrlUsername),
                            Req:respond({200, [{"Content-Type","text/html"}], Data});
                        false ->
			    case woomsg_following:is_following(Username, UrlUsername) of
			        true ->
				    %% 2.2 用户登录, 访问自己following人的页面
		                    Data = user_view:index(login, Username, ext_following, UrlUsername),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data});
				false ->
				    %% 2.3 用户登录, 访问陌生人人的页面
		                    Data = user_view:index(login, Username, ext_no_following, UrlUsername),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data})
			    end
                    end;
	        {logout_remember, undefined} ->
	            %% 用户没登录
		    Data = user_view:index(logout_remember, ?DEF_USERNAME, undefined, UrlUsername),
                    Req:respond({200, [{"Content-Type","text/html"}], Data});
                {logout_remember, Username} ->
	            %% 用户没登录
		    Data = user_view:index(logout_remember, Username, undefined, UrlUsername),
                    Req:respond({200, [{"Content-Type","text/html"}], Data});
		{logout_no_remember, undefined} ->
	            %% 用户没登录
		    Data = user_view:index(logout_no_remember, ?DEF_USERNAME, undefined, UrlUsername),
                    Req:respond({200, [{"Content-Type","text/html"}], Data})
            end;
	false ->
	    %% <1> 正在访问一个不存在的用户, 跳转到主页
            %% TODO: 单独的用户不存在页面
            Req:respond({302, [{"Location", "/"}], []})
    end.


handle_post(_Req) ->
    ok.

%% 解析出URL中的用户名: Username
%% 返回:
%% []
%% Username
%% Username?key=val   (错误的URL)
parse_username_from_url(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "user" ->
	    [];
	"user/" ->
	    [];
        _ ->	
            %% erlang:length("user/") = 5
            %% user/Username/xxx -> Username/xxx
            PathSuffix = lists:sublist(Path, 6, erlang:length(Path) - 5),
            
            %% Username/xxx     -> Username
            %% Username         -> Username
            %% Username?key=val -> Username?key=val (错误的用户名)
            %%
            %% 注意: 
            %% Usename?key=val这种形式的URL将返回错误的用户名
            %% 我们在设计中避免使用这样的URL
            woomsg_util:list_index_prefix($/, PathSuffix)
    end.
    
