-module(woomsg_image).
-include("woomsg_configure.hrl").
-export([get_user_photo_by_username/2,
         get_image_path/4,
	 delete_pic/1,
         safe_delete_pic/2,
	 delete_photo/3,
	 delete_pic/3,
         convert_photo/3,
         convert_pic/3,
	 convert/5]).

%% 根据用户名, 获取用户的头像路径.
%% Size: "ori" | "mini" | "normal"
%%
%% 返回值:
%% []
%% PhotoSrc
get_user_photo_by_username(Username, Size) ->
    case woomsg_user:get_user(Username) of
	{Username, _Password, _Email, PhotoGuid, PhotoPath, PhotoType} ->
	    PhotoPath ++ "/" ++ Size ++ "/" ++ PhotoGuid ++ PhotoType;
	_ ->
	    []
    end.

%% 适用于pic和photo
%% pic-Size: "ori" | "square" | "thumb" | "small" | "mediu"
%% photo-size: "ori" | "mini" | "normal"
get_image_path(Path, Guid, Type, Size) ->
    Path ++ "/" ++ Size ++ "/" ++ Guid ++ Type.

%% 删除一张照片(更新Mnesia & FileSystem)
%% (在删除之前会先确认照片的拥有者, 也就是只有照片的拥有者才能删除该照片)
%%
%% 返回值:
%% ok
%% {error, not_found}
%% {error, permission_error}
%% {error, mnesia_delete_pic_error}
%% {error, fs_delete_pic_error}
%%
safe_delete_pic(Guid, Owner) ->
    case woomsg_pic:get_pic(Guid) of
	[] ->
	    {error, not_found};
	{pic, Guid, Owner, Path, Type, _Msg, _Count, _Dig, _TagList, _Spam, _CreateDate} ->
	    case woomsg_pic:delete_pic(Guid) of
		error ->
		    {error, mnesia_delete_pic_error};
		ok ->
		    case delete_pic(Path, Guid, Type) of
			true ->
			    ok;
			false ->
			    {error, fs_delete_pic_error}
		    end
	    end;
	{pic, Guid, _Owner, _Path, _Type, _Msg, _Count, _Dig, _TagList, _Spam, _CreateDate} -> 
	    {error, permission_error}
    end.

%% 删除一张照片(更新Mnesia & FileSystem)
%% 
%% 返回值:
%% ok
%% {error, not_found}
%% {error, mnesia_delete_pic_error}
%% {error, fs_delete_pic_error}
%%
delete_pic(Guid) ->
    case woomsg_pic:get_pic(Guid) of
	[] ->
	    {error, not_found};
	{pic, Guid, _Owner, Path, Type, _Msg, _Count, _Dig, _TagList, _Spam, _CreateDate} ->
	    case woomsg_pic:delete_pic(Guid) of
		error ->
		    {error, mnesia_delete_pic_error};
		ok ->
		    case delete_pic(Path, Guid, Type) of
			true ->
			    ok;
			false ->
			    {error, fs_delete_pic_error}
		    end
	    end
    end.

%% 删除文件系统上的头像
delete_photo(Path, Guid, Type) ->
    NFSPath = ?NFS_PREFIX ++ Path,
    SourceFilename = NFSPath ++ "/ori/" ++ Guid ++ Type,
    MiniFilename = NFSPath ++ "/mini/" ++ Guid ++ Type,
    NormalFilename = NFSPath ++ "/normal/" ++ Guid ++ Type,
    ResSource = delete_file(SourceFilename),
    ResMini = delete_file(MiniFilename),
    ResNormal  = delete_file(NormalFilename),
    if
	ResSource =/= true ->
	    false;
	ResMini =/= true ->
	    false;
	ResNormal =/= true ->
	    false;
	true ->
	    true
    end.

%% 删除文件系统上的照片
delete_pic(Path, Guid, Type) ->
    NFSPath = ?NFS_PREFIX ++ Path,
    SourceFilename = NFSPath ++ "/ori/" ++ Guid ++ Type,
    SquareFilename = NFSPath ++ "/square/" ++ Guid ++ Type,
    ThumbFilename = NFSPath ++ "/thumb/" ++ Guid ++ Type,
    SmallFilename = NFSPath ++ "/small/" ++ Guid ++ Type,
    MediuFilename = NFSPath ++ "/mediu/" ++ Guid ++ Type,
    ResSource = delete_file(SourceFilename),
    ResSquare = delete_file(SquareFilename),
    ResThumb  = delete_file(ThumbFilename),
    ResSmall = delete_file(SmallFilename),
    ResMediu  = delete_file(MediuFilename),
    if
	ResSource =/= true ->
	    false;
	ResSquare =/= true ->
	    false;
	ResThumb =/= true ->
	    false;
	ResSmall =/= true ->
	    false;
	ResMediu =/= true ->
	    false;
	true ->
	    true
    end.


%% 根据SourceFile, 产生不同尺寸的头像图片.
%%
%% SourceFilename:
%% NFS_PREFIX/photo/node1/path-guid/ori/pic-guid.jpg
%% Path: /photo/node1/path-guid
%% Guid: pic-guid
%% Type: ".jpg"
%%
%% Type: ".png" | ".gif" | ".jpeg" | ".bmp"
convert_photo(Path, Guid, Type) ->
    NFSPath = ?NFS_PREFIX ++ Path,
    SourceFilename = NFSPath ++ "/ori/" ++ Guid ++ Type,
    ResSquare = convert(SourceFilename, NFSPath ++ "/mini/" ++ Guid ++ Type, 48, 48, true),
    ResThumb = convert(SourceFilename, NFSPath ++ "/normal/" ++ Guid ++ Type, 72, 72, true),
    if
        ResSquare =/= true ->
            false;
	ResThumb =/= true ->
            false;
        true ->
            true
    end.

%% 根据SourceFile, 产生不同尺寸的图片.
%%
%% SourceFilename:
%% NFS_PREFIX/pic/node1/path-guid/ori/pic-guid.jpg
%% Path: /pic/node1/path-guid
%% Guid: pic-guid
%% Type: ".jpg"
%%
%% Type: ".png" | ".gif" | ".jpeg" | ".bmp"
convert_pic(Path, Guid, Type) ->
    NFSPath = ?NFS_PREFIX ++ Path,
    SourceFilename = NFSPath ++ "/ori/" ++ Guid ++ Type,
    ResSquare = convert(SourceFilename, NFSPath ++ "/square/" ++ Guid ++ Type, 75, 75, true),
    ResThumb = convert(SourceFilename, NFSPath ++ "/thumb/" ++ Guid ++ Type, 150, 150, false),
    ResSmall = convert(SourceFilename, NFSPath ++ "/small/" ++ Guid ++ Type, 240, 240, false),
    ResMediu = convert(SourceFilename, NFSPath ++ "/mediu/" ++ Guid ++ Type, 500, 500, false),
    if
        ResSquare =/= true ->
            false;
	ResThumb =/= true ->
            false;
	ResSmall =/= true ->
	    false;
	ResMediu =/= true ->
	    false;
        true ->
            true
    end.

%% 原理:
%% <1> 不保持比例缩放:
%% convert -resize 400x400! src.jpg des.jpg
%% <2> 保持比例缩放(结果图片的Width=400, Height动态计算):
%% convert -resize 400x400 src.jpg des.jpg
%%
%% 图片的缩放函数:
%% @ Focus: true 不保持原图的比例; false 保持原图的比例
%%
convert(SourceFilename, TargetFilename, Width, Height, Focus) ->
    Cmd = case Focus of
	      true ->
		  io_lib:format("convert -resize ~px~p! ~s ~s", [Width, Height, SourceFilename, TargetFilename]);
	      false ->
		  io_lib:format("convert -resize ~px~p ~s ~s", [Width, Height, SourceFilename, TargetFilename])
	  end,
    case os:cmd(lists:flatten(Cmd)) of
        %% 返回结果的原理是:
	%% <1> 如果convert命令成功, 系统函数不返回任何结果, 所以os:cmd/1返回[]
        %% <2> 如果convert命令失败, 系统函数会返回错误信息, os:cmd/1会返回这些信息.
	[] ->
	    true;
	_ ->
	    false
    end.


%% Interlan APIs:

%% 删除文件
delete_file(Filename) ->
    case file:delete(Filename) of
	ok ->
	    true;
	{error, enoent} ->
	    true;
	_ ->
	    false
    end.
