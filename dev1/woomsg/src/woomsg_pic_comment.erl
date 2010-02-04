-module(woomsg_pic_comment).
-export([new_pic_comment/4, new_pic_comment/5,
         get_comment/1,
         delete_pic_comment/1,
         safe_delete_pic_comment/2,
         get_comment_all/1,
         get_comment_limit/2,
	 get_comment_limit/3]).

%% Comment:  6 - tuple
%% {pic_comment, Guid, PicGuid, Owner, Comment, CreateDate}

%% 根据Guid返回一条评论
%% 成功返回: {pic_comment, Guid, PicGuid, Owner, Comment, CreateDate}
%% 失败返回: {}
get_comment(Guid) ->
    F = fun() ->
            mnesia:read({pic_comment, Guid})
	end,
    case mnesia:transaction(F) of
	{atomic, [{pic_comment, Guid, PicGuid, Owner, Comment, CreateDate}]} ->
	    {pic_comment, Guid, PicGuid, Owner, Comment, CreateDate};
	_ ->
	    []
    end.

%% 增加一条评论:
%% (会自动更新pic表中图片的评论条数)
%% 成功返回: ok
%% 失败返回: error
new_pic_comment(Guid, PicGuid, Owner, Comment) ->
    CreateDate = woomsg_datetime:get_datetime(),
    new_pic_comment(Guid, PicGuid, Owner, Comment, CreateDate).

%% 增加一条评论:
%% (会自动更新pic表中图片的评论条数)
%% 成功返回: ok
%% 失败返回: error
new_pic_comment(Guid, PicGuid, Owner, Comment, CreateDate) ->
    F = fun() ->
            case mnesia:write({pic_comment, Guid, PicGuid, Owner, Comment, CreateDate}) of
                ok ->
		    %%更新pic表中的评论
		    case mnesia:read({pic, PicGuid}) of
		        [{pic, PicGuid, PicOwner, Path, Type, Msg, Count, Dig, TagList, Spam, PicCreateDate}] ->
		            mnesia:write({pic, PicGuid, PicOwner, Path, Type, Msg, Count + 1, Dig, TagList, Spam, PicCreateDate});
			 _ ->
			    mnesia:abort(read_pic_error)
                    end;
		_ ->
		    mnesia:abort(update_pic_comment_error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 删除一条评论:
%% (会自动更新pic表中图片的评论条数)
%% 成功返回: ok
%% 失败返回: error
delete_pic_comment(Guid) ->
    F = fun() ->
            case mnesia:read({pic_comment, Guid}) of
                [{pic_comment, Guid, PicGuid, _Owner, _Comment, _CreateDate}] ->
	            case mnesia:delete({pic_comment, Guid}) of
		        ok ->
		            %%更新pic表中的评论
		            case mnesia:read({pic, PicGuid}) of
		                [{pic, PicGuid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		                    mnesia:write({pic, PicGuid, Owner, Path, Type, Msg, Count - 1, Dig, TagList, Spam, CreateDate});
			        _ ->
			            mnesia:abort(read_pic_error)
                            end;
			_ ->
			    mnesia:abort(update_pic_comment_error)
                    end;
		_ ->
		    mnesia:abort(read_pic_comment_error)
	    end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 删除一条评论
%% (在删除之前会先确认评论的拥有者, 也就是只有评论的拥有者才能删除该评论)
%% (会自动更新pic表中图片的评论条数)
%% 成功返回: ok
%% 失败返回: error 
%%          {error, permission_error}
safe_delete_pic_comment(Guid, Owner) ->
    F = fun() ->
            case mnesia:read({pic_comment, Guid}) of
                [{pic_comment, Guid, PicGuid, Owner, _Comment, _CreateDate}] ->
	            case mnesia:delete({pic_comment, Guid}) of
		        ok ->
		            %%更新pic表中的评论
		            case mnesia:read({pic, PicGuid}) of
		                [{pic, PicGuid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		                    mnesia:write({pic, PicGuid, Owner, Path, Type, Msg, Count - 1, Dig, TagList, Spam, CreateDate});
			        _ ->
			            mnesia:abort(read_pic_error)
                            end;
			_ ->
			    mnesia:abort(update_pic_comment_error)
                    end;
		[{pic_comment, Guid, _PicGuid, _Owner, _Comment, _CreateDate}] ->
		    mnesia:abort(delete_permission_error);
		_ ->
		    mnesia:abort(read_pic_comment_error)
	    end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, delete_permission_error} ->
	    {error, permisson_error};
	_ ->
	    error
    end.

%% 根据照片的GUID(pic_guid)来返回评论
%% 如果存在: {count, Comment:list()} Comment = {pic_comment, Guid, PicGuid, Owner, Comment, CreateDate}
%% 如果没有: {0, []}
get_comment_all(PicGuid) ->
    F = fun() ->
	    mnesia:index_read(pic_comment, PicGuid, pic_guid)
	end,
    case mnesia:transaction(F) of
        {atomic, []} ->
	    {0, []};
	{atomic, ValList} when is_list(ValList) ->
	    {erlang:length(ValList), ValList};
	_ ->
	    {0, []}
    end.

get_comment_limit(PicGuid, Len) ->
    get_comment_limit(PicGuid, 1, Len).

%% 说明:
%% 这个API已经过时, 提供的Limit方法不能按照CreateDate排序, 
%% 上层的应用不能使用.
%%
%% 实现了SQL中的Limit的功能:
%% {Start从1开始}
%% 成功返回:                {count, Comment:list()}
%% 如果结果为空:            {0, []}
%% Start大于结果的最大长度: {out_of_index, []}
%% 发生错误:                {error, []}
get_comment_limit(PicGuid, Start, Len) ->
    F = fun() ->
	    mnesia:index_read(pic_comment, PicGuid, pic_guid)
	end,
    case mnesia:transaction(F) of
        {atomic, []} ->
	    {0, []};
	{atomic, ValList} when is_list(ValList) ->
	    case erlang:length(ValList) < Start of
	        true ->
		    {out_of_index, []};
		false ->
		    ResList = lists:sublist(ValList, Start, Len),
		    {erlang:length(ResList), ResList}
            end;
	_ ->
	    {0, []}
    end.
