-module(woomsg_register).
-include("woomsg_configure.hrl").
-export([is_registered/1, 
         register/3]).

%% 判断用户是否已经注册
%% 注册过: 返回true
%% 未注册: 返回false
is_registered(Username) ->
    woomsg_user:is_exist(Username).

%% 注册新用户
%% (密码使用MD5存储)
%% 成功返回: ok
%% 失败返回: error
register(Username, Password, Email) ->
    PhotoGuid = ?DEFAULT_PHOTO_GUID,
    PhotoPath = ?DEFAULT_PHOTO_PATH,
    PhotoType = ?DEFAULT_PHOTO_TYPE,
    Md5Pwd = woomsg_md5:md5_hex(Password),
    woomsg_user:new_user(Username, Md5Pwd, Email, PhotoGuid, PhotoPath, PhotoType).
