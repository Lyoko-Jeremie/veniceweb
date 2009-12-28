-module(woomsg_following).
-export([add_following/2,
         remove_following/2,
         get_following_limit/2,
         get_following_limit/3,
         get_following_all/1]).

%% 添加一个Following
%% 成功返回: ok
%% 失败返回: error
add_following(Username1, Username2) ->
    F = fun() ->
	    mnesia:write({following, Username1, Username2})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 删除一条{Username1, Username2}记录
%% 成功返回: ok
%% 失败返回: error
remove_following(Username1, Username2) ->
    F = fun() ->
	    mnesia:delete_object({following, Username1, Username2})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 获取Username1关注的所有用户
%% 存在返回:     {count, Username2:list()}
%% 没有或者失败: {0, []}
get_following_all(Username1) ->
    F = fun() ->
	    mnesia:read({following, Username1})
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {0, []};
	{atomic, ValList} when is_list(ValList) ->
	    UList = lists:foldl(fun({following, _Username1, Username2} = _Elem, AccIn) ->
		     	            [Username2 | AccIn]
	                        end, [], ValList),
	    {erlang:length(UList), UList};
        _ ->
	    {0, []}
    end.

get_following_limit(Username1, Len) ->
    get_following_limit(Username1, 1, Len).

%% 相当于实现了SQL中Limit的功能
%% (Start从1开始)
%% 成功返回:                {Count, Username2:list()}
%% 如果结果为空:            {null, []}
%% Start大于结果的最大长度, {out_of_index, []}
%% 发生错误:                {error, []}
get_following_limit(Username1, Start, Len) ->
    F = fun() ->
	    mnesia:read({following, Username1})
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {null, []};
	{atomic, ValList} when is_list(ValList) ->
	    UList = lists:foldl(fun({following, _Username1, Username2} = _Elem, AccIn) ->
		     	            [Username2 | AccIn]
	                        end, [], ValList),
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
