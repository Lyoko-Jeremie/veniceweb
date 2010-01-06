-module(login_controller).
-export([handle_get/1, handle_post/1]).

-define(USERNAME_KEY, "username").
-define(PASSWORD_KEY, "password").
-define(REMEMBER_KEY, "remember").

-define(REMEMBER_VAL, "1").

handle_get(Req) ->
    Data = login_view:index("test data"),
    Req:respond({200, [{"Content-Type","text/html"}], Data}).

%% PostPair - format:
%% [{"username","username"},{"password",[]},{"x","71"},{"y","19"}] 
%% [{"username","username"}, {"password","woomsg password"}, {"remember","1"}, {"x","56"}, {"y","33"}] 
handle_post(Req) ->
    PostPair = Req:parse_post(),
    Username = proplists:get_value(?USERNAME_KEY, PostPair),
    Password = proplists:get_value(?PASSWORD_KEY, PostPair),
    Remember = proplists:get_value(?REMEMBER_KEY, PostPair),

    case verify_post(Username, Password) of
	username_null ->
            Data = login_view:index("username_null"),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
	password_null ->
            Data = login_view:index("password_null"),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
        false ->
            Data = login_view:index("login_false"),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
	true ->
            %% 验证成功
            %% <1> 根据新的参数username/password/remember更新cookie
            %% <2> 跳转到用户Username主页
            CookieUsr = woomsg_cookie:get_cookie_of_username(Username),
            CookieSid = woomsg_cookie:get_cookie_of_sessionid(Username),
            CookieRem = woomsg_cookie:get_cookie_of_remember(Remember =:= ?REMEMBER_VAL),
            Req:respond({302, [{"Location", "user/" ++ Username}, CookieUsr, CookieSid, CookieRem], []})
    end.

%% 验证用户名和密码
%% 用户名为空:  username_null
%% 密码为空:    password_null
%% 验证失败:    false
%% 验证成功:    true
verify_post(Username, Password) ->
    case (Username =:= undefined) or (Username =:= []) of 
	true ->
	    username_null;
	false ->
	    case (Password =:= undefined) or (Password =:= []) of
	        true ->
		    password_null;
		false ->
		    %% 验证用户名和密码(成功返回true, 失败返回false)
	            woomsg_login:check_password(Username, Password)	    
	    end
    end.
