-module(showpicfull_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).

handle_get(Req) ->
    PicGuid = parse_guid_from_url(Req),
    case woomsg_pic:get_pic(PicGuid) of
	[] ->
	    %% 照片不存在, 返回到主页
	    Req:respond({302, [{"Location", "/"}], []});
	{pic, PicGuid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate} ->
	    PhotoInfo = {PicGuid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate},
	    case woomsg_common:user_state(Req) of
		{login, Username} ->
		    %% 用户登录
		    Data = showpicfull_view:index(login, Username, PhotoInfo),
		    Req:respond({200, [{"Content-Type", "text/html"}], Data});
		{logout_remember, undefined} ->
		    %% 用户没登录
		    Data = showpicfull_view:index(logout_remember, ?DEF_USERNAME, PhotoInfo),
		    Req:respond({200, [{"Content-Type", "text/html"}], Data});
		{logout_remember, Username} ->
		    %% 用户没登录
		    Data = showpicfull_view:index(logout_remember, Username, PhotoInfo),
		    Req:respond({200, [{"Content-Type", "text/html"}], Data});
		{logout_no_remember, undefined} ->
		    %% 用户没登录
		    Data = showpicfull_view:index(logout_no_remember, ?DEF_USERNAME, PhotoInfo),
		    Req:respond({200, [{"Content-Type", "text/html"}], Data})
            end
    end.

handle_post(_Req) ->
    ok.

%% 解析出URL中的GUID: GUID
%% 返回:
%% []
%% GUID
%% GUID?key=val   (错误的URL)
parse_guid_from_url(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "showpicfull" ->
	    [];
	"showpicfull/" ->
	    [];
        _ ->	
            %% erlang:length("showpicfull/") = 12
            %% showpicfull/GUID/xxx -> GUID/xxx
            PathSuffix = lists:sublist(Path, 13, erlang:length(Path) - 12),
            
            %% GUID/xxx     -> GUID
            %% GUID         -> GUID
            %% GUID?key=val -> GUID?key=val (错误的用户名)
            %%
            woomsg_util:list_index_prefix($/, PathSuffix)
    end.
