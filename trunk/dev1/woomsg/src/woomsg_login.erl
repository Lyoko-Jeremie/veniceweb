-module(woomsg_login).
-export([check_password/2]).

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
