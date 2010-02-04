-module(woomsg_pic).
-export([get_all/0,
	 get_pic_all_by_guids/1,
	 get_pic_all_by_owners/1,
         new_pic/5,
         get_pic/1,
         get_pic_all/1,
         get_pic_limit/2,
         get_pic_limit/3,
         delete_pic/1,
         inc_count/1, inc_count/2, dec_count/1, dec_count/2,
         inc_dig/1, inc_dig/2, dec_dig/1, dec_dig/2,
         set_spam/2,
	 add_tag/2, add_tags/2, safe_add_tag/3, safe_add_tags/3,
	 delete_tag/2, safe_delete_tag/3]).

%% pic 是一个: 11 tuple
%% {pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}

%% 返回所有的照片
%% 如果存在: 返回{count, PicList}
%% 如果没有: {0, []}
get_all() ->
    F = fun() ->
	    mnesia:match_object({pic, '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'})
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {0, []};
	{atomic, ValList} when is_list(ValList) ->
	    {erlang:length(ValList), ValList};
	_ ->
	    {0, []}
    end.

%% 根据picguid列表返回照片
%% (Mnesia有没有更高效的方式实现SQL中的类似: select ... in 功能?)
%%
%% 如果存在: 返回{count, PicList}
%% 如果没有: {0, []}
get_pic_all_by_guids([]) ->
    {0, []};
get_pic_all_by_guids(Guids) when is_list(Guids) ->
    F = fun() ->
	    lists:foldl(fun(Guid, AccIn) ->
			    %% read/3 将返回[]或者PicList
			    mnesia:read({pic, Guid}) ++ AccIn
			end, [], Guids)
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {0, []};
	 {atomic, ValList} when is_list(ValList) ->
	    {erlang:length(ValList), ValList};
	_ ->
	    {0, []}
    end;
get_pic_all_by_guids(_) ->
    {0, []}.

%% 根据用户名列表返回照片
%% (Mnesia有没有更高效的方式实现SQL中的类似: select ... in 功能?)
%%
%% 如果存在: 返回{count, PicList}
%% 如果没有: {0, []}
get_pic_all_by_owners([]) ->
    {0, []};
get_pic_all_by_owners(Owners) when is_list(Owners) ->
    F = fun() ->
	    lists:foldl(fun(Owner, AccIn) ->
			    %% index_read/3 将返回[]或者PicList
			    mnesia:index_read(pic, Owner, owner) ++ AccIn
			end, [], Owners)
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {0, []};
	 {atomic, ValList} when is_list(ValList) ->
	    {erlang:length(ValList), ValList};
	_ ->
	    {0, []}
    end;
get_pic_all_by_owners(_) ->
    {0, []}.

%% 根据照片的Owner来查找照片
%% 如果存在: 返回{count, PicList}
%%   Pic = {pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}
%% 如果没有: {0, []}
get_pic_all(Owner) ->
    F = fun() ->
            mnesia:index_read(pic, Owner, owner)
        end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {0, []};
	{atomic, ValList} when is_list(ValList) ->
	    {erlang:length(ValList), ValList};
	_ ->
	    {0, []}
    end.

get_pic_limit(Owner, Len) ->
    get_pic_limit(Owner, 1, Len).

%% 实现了SQL中的Limit功能:
%% {Start从1开始}
%% 成功返回:                 {count, Pic:list()}     
%% 如果结果为空:             {0, []}
%% Start大于结果的最大长度:  {out_of_index, []}
%% 发生错误:                 {error, []}
get_pic_limit(Owner, Start, Len) ->
    F = fun() ->
	    mnesia:index_read(pic, Owner, owner)
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
	    {error, []}
    end.


%% 使用建议:
%% <1>不建议单独使用inc_count/1, inc_count/2, dec_count/1, dec_count/2
%%    来修改评论,图片的评论数会在修改pic_comment的事务中自动更新.
%%

%% 根据图片的GUID返回一张图片
%% 成功返回: {}
%% 失败返回: []
get_pic(Guid) ->
    F = fun() ->
	    mnesia:read({pic, Guid})
	end,
    case mnesia:transaction(F) of
	{atomic, [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}]} ->
	    {pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate};
	_ ->
	    []
    end.

%% 增加一张照片
%% 成功返回: ok
%% 失败返回: error
new_pic(Guid, Owner, Path, Type, Msg) ->
    CreateDate = woomsg_datetime:get_datetime(),
    F = fun() ->
	    mnesia:write({pic, Guid, Owner, Path, Type, Msg, 0, 0, [], 0, CreateDate})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% TODO: 这个函数存在Bug, 需要修正
%%        
%% 删除一张照片:
%% (会自动把照片相关的评论一起删除)
%% 成功返回: ok
%% 失败返回: error
%%
%% 补充:
%% 我们使用mnesia:index_match_object(Pattern, Pos)
%% 来代替mnesia:match_object(Pattern)
%% 理由是: 使用前者可以利用索引的信息来加速匹配
delete_pic(Guid) ->
    F = fun() ->
	    case mnesia:delete({pic, Guid}) of
                ok ->
		    %% 更新pic_comment表, 删除所有的评论信息
                    case mnesia:index_match_object({pic_comment, '_', Guid, '_', '_', '_'}, pic_guid) of 
			[] ->
			    ok;
			ValList when is_list(ValList)->
			    lists:foreach(fun({pic_comment, CommentGuid, _Guid, _, _, _} = _Elem) ->
					      mnesia:delete({pic_comment, CommentGuid})
					  end, ValList),
			    ok;
			_ ->
			    mnesia:abort(error)
                    end;
		_ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

inc_count(Guid) ->
    inc_count(Guid, 1).

%% 增加评论的数量
%% 成功返回: ok
%% 失败返回: error
inc_count(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count + Num, Dig, TagList, Spam, CreateDate});
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

dec_count(Guid) ->
    dec_count(Guid, 1).

%% 减少评论的数量:
%% (如果要减少的数量>当前的评论书, 则评论重置为0)
%% 成功返回: ok
%% 失败返回: error
dec_count(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    case Count > Num of
		        true ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count - Num, Dig, TagList, Spam, CreateDate});
			false ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, 0, Dig, TagList, Spam, CreateDate})
                    end; 
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
    
inc_dig(Guid) ->
    inc_dig(Guid, 1).

%% 增加dig的数量
%% 成功返回: ok
%% 失败返回: error
inc_dig(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig + Num, TagList, Spam, CreateDate});
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

dec_dig(Guid) ->
    dec_dig(Guid, 1).

%% 减少dig的数量:
%% (如果要减少的数量>当前的dig数, 则dig重置为0)
%% 成功返回: ok
%% 失败返回: error
dec_dig(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    case Dig > Num of
		        true ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig - Num, TagList, Spam, CreateDate});
			false ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, 0, TagList, Spam, CreateDate})
                    end; 
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 设置图片是否为垃圾信息
%% true  - 垃圾信息
%% false - 正常图片
%% 成功返回: ok
%% 失败返回: error
set_spam(Guid, ArgSpam) ->
    ResSpam = case ArgSpam =:= true of
                  true ->
		      1;
	          _ ->
		      0
              end,
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, _Spam, CreateDate}] ->
                     mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, ResSpam, CreateDate});
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 增加一个tag
%% 成功返回: ok
%% 失败返回: error
add_tag(Guid, Tag) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    NewTagList = case lists:member(Tag, TagList) of
				     true ->
					 TagList;
				     false ->
					 [Tag | TagList]
				 end,
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, NewTagList, Spam, CreateDate});
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 增加多个tag
%% 成功返回: ok
%% 失败返回: error
add_tags(Guid, Tags) when is_list(Tags) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    NewTagList = lists:foldl(fun(Elem, AccIn) ->
					         case lists:member(Elem, AccIn) of
						     true ->
							 AccIn;
						     false ->
							 [Elem | AccIn]
						 end
					     end, TagList, Tags),
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, NewTagList, Spam, CreateDate});
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end;
add_tags(_Guid, _Tag) ->
    error.

%% 增加一个tag
%% 成功返回: ok
%% 失败返回: error
%%          {error, permission_error}
safe_add_tag(Guid, Tag, Owner) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    NewTagList = case lists:member(Tag, TagList) of
				     true ->
					 TagList;
				     false ->
					 [Tag | TagList]
				 end,
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, NewTagList, Spam, CreateDate});
		[{pic, Guid, _Owner, _Path, _Type, _Msg, _Count, _Dig, _TagList, _Spam, _CreateDate}] ->
		    mnesia:abort(add_permission_error);
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	{aborted, add_permission_error} ->
	    {error, permission_error};
	_ ->
	    error
    end.

%% 增加多个tag
%% 成功返回: ok
%% 失败返回: error
%%          {error, permission_error}
safe_add_tags(Guid, Tags, Owner) when is_list(Tags) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    NewTagList = lists:foldl(fun(Elem, AccIn) ->
					         case lists:member(Elem, AccIn) of
						     true ->
							 AccIn;
						     false ->
							 [Elem | AccIn]
						 end
					     end, TagList, Tags),
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, NewTagList, Spam, CreateDate});
		[{pic, Guid, _Owner, _Path, _Type, _Msg, _Count, _Dig, _TagList, _Spam, _CreateDate}] ->
		    mnesia:abort(add_permission_error);
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	{aborted, add_permission_error} ->
	    {error, permission_error};
	_ ->
	    error
    end;
safe_add_tags(_Guid, _Tag, _Owner) ->
    error.

%% 删除一个tag
%% 成功返回: ok
%% 失败返回: error
delete_tag(Guid, Tag) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    NewTagList = lists:delete(Tag, TagList),
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, NewTagList, Spam, CreateDate});
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 删除一个tag
%% 成功返回: ok
%% 失败返回: error
%%          {error, permission_error}
safe_delete_tag(Guid, Tag, Owner) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    NewTagList = lists:delete(Tag, TagList),
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, NewTagList, Spam, CreateDate});
		[{pic, _Guid, _Owner, _Path, _Type, _Msg, _Count, _Dig, _TagList, _Spam, _CreateDate}] ->
		    mnesia:abort(delete_permission_error);
	        _ ->
		    mnesia:abort(error)
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	{aborted, delete_permission_error} ->
	    {error, permission_error};
	_ ->
	    error
    end.
