-module(woomsg_login).
-export([check_password/2,
         auth/1]).

%% 验证密码:
%% 成功返回: true
%% 失败返回: false
check_password(Username, Password) ->
    Md5Pwd = woomsg_md5:md5_hex(Password),
    case woomsg_user:get_user(Username) of
	{Username, Md5Pwd, _Email, _PhotoGuid, _PhotoPath, _PhotoType} ->
	    true;
	_ ->
	    false
    end.

%% 检测Cookie中的数据, 判断用户是否登录
%% <1> 用户已经登录
%% <2> 用户没有登录
%%
%% 返回值
%% <1> {login, CookieUsr}   CookieUsr就是用户名
%% <2> {logout, CookieUsr}
auth(Req) ->
    CookieUsr = woomsg_cookie:parse_cookie_username(Req),
    case woomsg_cookie:check_session_login(Req, CookieUsr) of
        true ->
            %% 通过检测cookie中的数据 -> 用户已经登录
            {login, CookieUsr};
        false ->
            {logout, CookieUsr}
    end.  
