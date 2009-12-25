-module(woomsg_register).
-include("woomsg_configure.hrl").
-export([is_registered/1, register/3]).

%% 判断用户是否已经注册
is_registered(_Username) ->
    true.

%% 注册新用户
register(Username, Password, Email) ->
    F = fun() ->
	    Photo = ?DEFAULT_PHOTO,
	    CreateData = woomsg_datetime:get_datetime(),
	    mnesia:write({user, Username, Password, Email, Photo, CreateData})
	end,
    mnesia:transaction(F).
