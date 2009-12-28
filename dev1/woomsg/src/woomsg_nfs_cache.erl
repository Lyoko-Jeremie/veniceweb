-module(woomsg_nfs_cache).
-include("woomsg_configure.hrl").
-export([set_photo_path/1,
         set_pic_path/1,
         get_photo_path/0,
         get_pic_path/0]).

-define(PIC_KEY, pic).
-define(PHOTO_KEY, photo).

%% 设置新的NFS-photo的存储路径
%% 成功返回: ok
%% 失败返回: error
set_photo_path(Path) ->
    F = fun() ->
	    mnesia:write({nfs_cache, ?PHOTO_KEY, Path})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.


%% 设置新的NFS-pic的存储路径
%% 成功返回: ok
%% 失败返回: error
set_pic_path(Path) ->
    F = fun() ->
	    mnesia:write({nfs_cache, ?PIC_KEY, Path})
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 获取NFS-photo对应的当前路径
%% 成功返回: Path
%% 失败返回: []
get_photo_path() ->
    F = fun() ->
	    mnesia:read({nfs_cache, ?PHOTO_KEY})
	end,
    case mnesia:transaction(F) of
	{atomic, [{nfs_cache, ?PHOTO_KEY, Path}]} ->
	    Path;
	_ ->
	    []
    end.

%% 获取NFS-pic对应的当前路径
%% 成功返回: Path
%% 失败返回: []
get_pic_path() ->
    F = fun() ->
            mnesia:read({nfs_cache, ?PIC_KEY})
	end,
    case mnesia:transaction(F) of
	{atomic, [{nfs_cache, ?PIC_KEY, Path}]} ->
	    Path;
	_ ->
	    []
    end.



