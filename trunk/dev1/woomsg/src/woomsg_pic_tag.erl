-module(woomsg_pic_tag).
-export([add_tag/2,
	 add_tags/2,
         remove_tag/2,
         get_guid_all/1,
         get_guid_limit/2,
         get_guid_limit/3]).

%% 添加一个Guid到一个Tag
%% 成功返回: ok
%% 失败返回: error
add_tag(Tag, Guid) ->
    F = fun() ->
	    mnesia:write({pic_tag, Tag, Guid})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 添加多条记录
%% 成功返回: ok
%% 失败返回: error
add_tags(Tags, Guid) when is_list(Tags) ->
    F = fun() ->
	    lists:foreach(fun(Elem) ->
			      mnesia:write({pic_tag, Elem, Guid})
			  end, Tags)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end;
add_tags(_Tags, _Guid) ->
    error.

%% 删除一个{Tag, Guid}Object
%% 成功返回: ok
%% 失败返回: error
remove_tag(Tag, Guid) ->
    F = fun() ->
	    mnesia:delete_object({pic_tag, Tag, Guid})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 获取Tag对应的所有的GUID
%% 存在返回:     {count, Guid:list()}
%% 没有或者失败: {0, []}
get_guid_all(Tag) ->
    F = fun() ->
	    mnesia:read({pic_tag, Tag})
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {0, []};
	{atomic, ValList} when is_list(ValList) ->
	    UList = lists:foldl(fun({pic_tag, _Tag, Guid} = _Elem, AccIn) ->
                                    [Guid | AccIn]
				end, [], ValList ),
            {erlang:length(UList), UList};
	_ ->
	    {0, []}
    end.

get_guid_limit(Tag, Len) ->
    get_guid_limit(Tag, 1, Len).

%% 相当于实现了SQL中的Limit的功能
%% (Start从1开始)
%% 成功返回:                {Count, Guid:list()}
%% 结果为空:                {null, []}
%% Start大于结果的最大长度: {out_of_index, []}
%% 发生错误:                {error, []}
get_guid_limit(Tag, Start, Len) ->
    F = fun() ->
	    mnesia:read({pic_tag, Tag})
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {null, []};
	{atomic, ValList} when is_list(ValList) ->
	    UList = lists:foldl(fun({pic_tag, _Tag, Guid} = _Elem, AccIn) ->
                                    [Guid | AccIn]
				end, [], ValList ),
	    case erlang:length(UList) < Start of
	        true ->
		    {out_of_index, []};
		false ->
		    ResList = lists:sublist(UList, Start, Len),
		    {erlang:length(ResList), ResList}
            end;
        _ ->
	    {error, []}
    end.
