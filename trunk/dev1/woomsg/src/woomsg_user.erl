-module(woomsg_user).
-export([is_exist/1,
         new_user/6,
         get_user/1,
         get_user_ext/1, 
         get_user_all/1, 
         set_password/2,
         set_photo/4,
         delete_user/1]).

%% 判断用户是否存在:
%% 存在返回: true
%% 失败返回: false
is_exist(Username) ->
    F = fun() ->
	    case mnesia:read({user, Username}) of
                [{user, Username, _Password, _Email, _PhotoGuid, _PhotoPath, _PhotoType, _CreateDate}] ->
		    true;
		_ ->
		    false
            end
	end,
    case mnesia:transaction(F) of
        {atomic, true} ->
	    true;
	_ ->
	    false
    end.

%% 创建一个信用户
%% (会同时更新user和user_ext两张表)
%% 成功返回: ok
%% 失败返回: error
new_user(Username, Password, Email, PhotoGuid, PhotoPath, PhotoType) ->
    F = fun() ->
	    CreateDate = woomsg_datetime:get_datetime(),
	    mnesia:write({user, Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, CreateDate}),
            mnesia:write({user_ext, Username, "", "", "", "", ""})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 获取用户的基本信息
%% 失败返回: []
%% 成功返回: {Username, Password, Email, PhotoGuid ,PhotoPath, PhotoType}
get_user(Username) ->
    F = fun() ->
	    mnesia:read({user, Username})
	end,
    case mnesia:transaction(F) of
	{atomic, [{user, Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, _CreateDate}]} ->
	    {Username, Password, Email, PhotoGuid, PhotoPath, PhotoType};
	_ ->
	    []
    end.

%% 获取用户的扩展信息
%% 失败返回: []
%% 成功返回: {Username, Fullname, Sex, Location, Web, Describe}
get_user_ext(Username) ->
    F = fun() ->
	    mnesia:read({user_ext, Username})
	end,
    case mnesia:transaction(F) of
        {atomic, [{user_ext, Username, Fullname, Sex, Location, Web, Describe}]} ->
	    {Username, Fullname, Sex, Location, Web, Describe};
	_ ->
	    []
    end.

%% 获取用户的所有信息(基本信息 + 扩展信息)
%% 失败返回: []
%% 成功返回: {Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, Fullname, Sex, Location, Web, Describe}
get_user_all(Username) ->
    F = fun() ->
            case mnesia:read({user, Username}) of
	        [{user, Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, _CreateData}] ->
	            case mnesia:read({user_ext, Username}) of
                        [{user_ext, Username, Fullname, Sex, Location, Web, Describe}] ->
	                    {Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, Fullname, Sex, Location, Web, Describe};
	                _ ->
	                    []
                    end;
                _ ->
	            []
            end
        end,
    case mnesia:transaction(F) of
        {atomic, {Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, Fullname, Sex, Location, Web, Describe}} ->
	    {Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, Fullname, Sex, Location, Web, Describe};
	_ ->
	    []
    end.

%% 设置(修改密码)
%% 成功返回: ok
%% 失败返回: error
set_password(Username, Password) ->
    F = fun() ->
            case mnesia:read({user, Username}) of
                [{user, Username, _OldPassword, Email, PhotoGuid, PhotoPath, PhotoType, CreateDate}] ->
		    mnesia:write({user, Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, CreateDate});
 		_ ->
		    error
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 设置(修改头像)
%% 成功返回: ok
%% 失败返回: error
set_photo(Username, PhotoGuid, PhotoPath, PhotoType) ->
    F = fun() ->
            case mnesia:read({user, Username}) of
                [{user, Username, Password, Email, _OldPhotoGuid, _OldPhotoPath, _OldPhotoType, CreateDate}] ->
		    mnesia:write({user, Username, Password, Email, PhotoGuid, PhotoPath, PhotoType, CreateDate});
 		_ ->
		    error
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 删除用户的信息(user和user_ext)
%% 成功返回: ok
%% 失败返回: error
delete_user(Username) ->
    F = fun() ->
	    mnesia:delete({user, Username}),
 	    mnesia:delete({user_ext, Username})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
