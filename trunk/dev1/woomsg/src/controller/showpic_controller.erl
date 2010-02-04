-module(showpic_controller).
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
	    OwnerInfo = case woomsg_user:get_user(Owner) of
			    [] ->
				{Owner, undefined, undefined, undefined};
			    {Owner, _Password, _Email, PhotoGuid ,PhotoPath, PhotoType} ->
				{Owner, PhotoGuid, PhotoPath, PhotoType};
			    _ ->
				{Owner, undefined, undefined, undefined}
			end,
            CommentList = get_comment_list(PicGuid, Count),
	    {PicCount, PicList} = woomsg_pic:get_pic_all(Owner),
	    ResPicList = woomsg_pic_hook:process_pic_simple_limit({PicCount, PicList}, 3),
	    case woomsg_common:user_state(Req) of
		{login, Username} ->
		    %% 用户登录
		    %% (格式化评论信息)
		    ResCommentList = woomsg_comment_hook:process_comment(CommentList, Username),
		    Data = showpic_view:index(login, Username, PhotoInfo, ResCommentList, OwnerInfo, ResPicList),
		    Req:respond({200, [{"Content-Type", "text/html"}], Data});
		{logout_remember, undefined} ->
		    %% 用户没登录
		    ResCommentList = woomsg_comment_hook:process_comment(CommentList, undefined),
		    Data = showpic_view:index(logout_remember, ?DEF_USERNAME, PhotoInfo, ResCommentList, OwnerInfo, ResPicList),
		    Req:respond({200, [{"Content-Type", "text/html"}], Data});
		{logout_remember, Username} ->
		    %% 用户没登录
		    ResCommentList = woomsg_comment_hook:process_comment(CommentList, Username),
		    Data = showpic_view:index(logout_remember, Username, PhotoInfo, ResCommentList, OwnerInfo, ResPicList),
		    Req:respond({200, [{"Content-Type", "text/html"}], Data});
		{logout_no_remember, undefined} ->
		    %% 用户没登录
		    ResCommentList = woomsg_comment_hook:process_comment(CommentList, undefined),
		    Data = showpic_view:index(logout_no_remember, ?DEF_USERNAME, PhotoInfo, ResCommentList, OwnerInfo, ResPicList),
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
        "showpic" ->
	    [];
	"showpic/" ->
	    [];
        _ ->	
            %% erlang:length("showpic/") = 8
            %% showpic/GUID/xxx -> GUID/xxx
            PathSuffix = lists:sublist(Path, 9, erlang:length(Path) - 8),
            
            %% GUID/xxx     -> GUID
            %% GUID         -> GUID
            %% GUID?key=val -> GUID?key=val (错误的用户名)
            %%
            woomsg_util:list_index_prefix($/, PathSuffix)
    end.
    
%% 获取照片的评论
get_comment_list(PicGuid, Count) ->
    case Count =:= 0 of
        true ->
	    {0, []};
	false ->
	    case woomsg_pic_comment:get_comment_all(PicGuid) of
	        {0, []} ->
	            {0, []};
	        {Count, CommentList} ->
	            {Count, CommentList};
	        _ ->
	            {0, []}
	    end
     end.
