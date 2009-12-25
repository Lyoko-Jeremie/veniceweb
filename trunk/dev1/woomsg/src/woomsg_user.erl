-module(woomsg_user).
-export([get_user/1, get_user_ext/1, get_user_all/1, delete_user/1]).

%% 获取用户的基本信息
%% 失败返回: []
%% 成功返回: {Username, Password, Email, Photo}
get_user(Username) ->
    case db:find(user, Username) of
	{atomic, [{user, Username, Password, Email, Photo, _CreateDate}]} ->
	    {Username, Password, Email, Photo};
	_ ->
	    []
    end.

%% 获取用户的扩展信息
%% 失败返回: []
%% 成功返回: {Username, Fullname, Sex, Location, Web, Describe}
get_user_ext(Username) ->
    case db:find(user_ext, Username) of
        {atomic, [{user_ext, Username, Fullname, Sex, Location, Web, Describe}]} ->
	    {Username, Fullname, Sex, Location, Web, Describe};
	_ ->
	    []
    end.

%% 获取用户的所有信息(基本信息 + 扩展信息)
%% 失败返回: []
%% 成功返回: {Username, Password, Email, Photo, Fullname, Sex, Location, Web, Describe}
get_user_all(Username) ->
    F = fun() ->
            case mnesia:read({user, Username}) of
	        [{user, Username, Password, Email, Photo, _CreateData}] ->
	            case mnesia:read({user_ext, Username}) of
                        [{user_ext, Username, Fullname, Sex, Location, Web, Describe}] ->
	                    {Username, Password, Email, Photo, Fullname, Sex, Location, Web, Describe};
	                _ ->
	                    []
                    end;
                _ ->
	            []
            end
        end,
    case mnesia:transaction(F) of
        {atomic, {Username, Password, Email, Photo, Fullname, Sex, Location, Web, Describe}} ->
	    {Username, Password, Email, Photo, Fullname, Sex, Location, Web, Describe};
	_ ->
	    []
    end.

%% 删除用户的信息(user和user_ext)
delete_user(Username) ->
    F = fun() ->
	    mnesia:delete({user, Username}),
 	    mnesia:delete({user_ext, Username})
	end,
    mnesia:transaction(F).
