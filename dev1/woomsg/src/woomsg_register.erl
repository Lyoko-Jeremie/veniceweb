-module(woomsg_register).
-include("woomsg_configure.hrl").
-export([is_registered/1, register/3]).

%% 判断用户是否已经注册
is_registered(Username) ->
    case db:get(user, Username, username) of
        {atomic, Username} ->
	    true;
	_ ->
	    false
    end.

%% 注册新用户
%% 更新user和user_ext两张表
register(Username, Password, Email) ->
    F = fun() ->
	    Photo = ?DEFAULT_PHOTO,
	    CreateData = woomsg_datetime:get_datetime(),
	    mnesia:write({user, Username, Password, Email, Photo, CreateData}),
            mnesia:write({user_ext, Username, "", "", "", "", ""})
	end,
    mnesia:transaction(F).
