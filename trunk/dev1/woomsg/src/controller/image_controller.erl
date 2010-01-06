-module(image_controller).
-include("../woomsg_configure.hrl").
-export([handle_get/1, handle_post/1]).

handle_get(Req) ->
    case parse_size_from_url(Req) of
	{Size, Guid} ->
	    case woomsg_pic:get_pic(Guid) of
	        {Guid, _Owner, Path, Type, _Msg, _Count, _Dig, _TagList, _Spam, _CreateDate} ->
		    "/" ++ PicSrc = Path ++ "/" ++ Size ++ "/" ++ Guid ++ Type,
		    Req:serve_file(PicSrc, ?NFS_PREFIX);
		    %%Req:serve_file("test/mini-pic.jpg", ?NFS_PREFIX);
		_ ->
		    Req:not_found()
            end;
	_ ->
	    %% TODO: 返回一张not found的图片
	    Req:not_found()
    end.

handle_post(_Req) ->
    ok.


%% URL: pic/<Size>/Guid
parse_size_from_url(Req) ->
    "/" ++ Path = Req:get(path),
    case string:tokens(Path, "/") of
	["image", "ori", Guid] ->
	    {"ori", Guid};
	["image", "mediu", Guid] ->
	    {"mediu", Guid};
	["image", "small", Guid] ->
	    {"small", Guid};
	["image", "thumb", Guid] ->
	    {"thumb", Guid};
	["image", "square", Guid] ->
	    {"square", Guid};
	_ ->
	    undefined
    end.
