-module(woomsg_image).
-include("woomsg_configure.hrl").
-export([get_image_path/4,
         convert_photo/3,
         convert_pic/3,
	 convert/5]).

%% 适用于pic和photo
%% pic-Size: "ori" | "square" | "thumb" | "small" | "mediu"
%% photo-size: "ori" | "mini" | "normal"
get_image_path(Path, Guid, Type, Size) ->
    Path ++ "/" ++ Size ++ "/" ++ Guid ++ Type.

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