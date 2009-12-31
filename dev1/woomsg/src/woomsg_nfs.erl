-module(woomsg_nfs).
-include("woomsg_configure.hrl").
-export([make_dir_photo/1,
         make_dir_pic/1]).

%% NFS文件系统结构:
%%
%% NFS_PREFIX: 
%% /home/woomsg/fs
%%                /photo 存放头像 
%%                /pic   存放图片
%%              
%%                (三个子目录)
%%                /photo/sys/path-guid/mini     - 系统头像
%%                /photo/sys/path-guid/normal   - 系统头像
%%                /photo/sys/path-guid/ori      - 系统头像原始版(空目录)
%%
%%                (三个子目录)
%%                /photo/nodeX/path-guid/mini   - 用户上传的头像
%%                /photo/nodeX/path-guid/normal - 用户上传的头像
%%                /photo/nodeX/path-guid/ori    - 用户上传的头像的原始图
%%
%%                (五个子目录)
%%                /pic/nodeX/path-guid/square   - 用户上传的照片
%%                /pic/nodeX/path-guid/thumb    - 用户上传的照片
%%                /pic/nodeX/path-guid/small    - 用户上传的照片
%%                /pic/nodeX/path-guid/mediu    - 用户上传的照片
%%                /pic/nodeX/path-guid/orib     - 用户上传的照片
%%
%% 头像路径:
%%  Path:  /photo/nodeX/path-guid
%%  Size:  mini | normal | ori
%%  Type:  ".gif" | ".jpg" | ".png"
%%  ?NFS_PREFIX ++ Path ++ "/" ++ Size ++ "/" ++ Guid ++ Type
%%
%% 图片路径:
%%  Path:  /pic/nodeX/path-guid
%%  Size:  square | thumb | small | mediu | ori
%%  Type:  ".gif" | ".jpg" | ".png"
%%  ?NFS_PREFIX ++ Path ++ "/" ++ Size ++ "/" ++ Guid ++ Type
%%


%% 创建一个存放Photo的文件夹
%% /home/woomsg/fs
%%               /photo/Node/NewGUID
%%
%% (PHOTO_SIZES - 子目录)
%% 注意:
%%   Node必须是存在的Node, 否则返回{error, enoent}
%% 如果成功返回: {ok, /photo/Node/NewGUID}
%% 如果失败返回: {error, Reason}
make_dir_photo(Node) ->
    Suffix = "/photo/" ++ Node ++ "/" ++ woomsg_guid:get_path_guid(),
    case file:make_dir(?NFS_PREFIX ++ Suffix) of
        ok ->
	    FailedSizePath = lists:foldl(fun(Size, AccIn) ->
                                             case file:make_dir(?NFS_PREFIX ++ Suffix ++ "/"  ++ Size) of
					         ok ->
						     [];
						 {error, _} ->
						     [Size | AccIn]
                                             end
                                         end, [], ?PHOTO_SIZES),
	    case FailedSizePath of
	        [] ->
		    {ok, Suffix};
	        _ ->
		    {error, FailedSizePath}
            end;
	{error, Reason} ->
	    {error, Reason}
    end.

%% 创建一个存放Pic的文件夹
%% /home/woomsg/fs
%%               /pic/Node/NewGUID
%%
%% (PIC_SIZES - 子目录)
%% 注意:
%%   Node必须是存在的Node, 否则返回{error, enoent}
%% 如果成功返回: {ok, /pic/Node/NewGUID}
%% 如果失败返回: {error, Reason}
make_dir_pic(Node) ->
    Suffix = "/pic/" ++ Node ++ "/" ++ woomsg_guid:get_path_guid(),
    case file:make_dir(?NFS_PREFIX ++ Suffix) of
        ok ->
	    FailedSizePath = lists:foldl(fun(Size, AccIn) ->
                                             case file:make_dir(?NFS_PREFIX ++ Suffix ++ "/" ++  Size) of
					         ok ->
						     [];
						 {error, _} ->
						     [Size | AccIn]
                                             end
                                         end, [], ?PIC_SIZES),
	    case FailedSizePath of
	        [] ->
		    {ok, Suffix};
	        _ ->
		    {error, FailedSizePath}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
    


