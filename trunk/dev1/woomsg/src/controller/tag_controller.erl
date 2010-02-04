-module(tag_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).
-define(DEF_PAGE_SIZE, 25).

%% 客户端POST的数据Keys
-define(PIC_GUID_KEY, "pic-guid").
-define(PIC_TAG_KEY, "pic-tag").
-define(PIC_TAGS_KEY, "pic-tags").

%% JSON - 返回价值
-define(RESULT_KEY, <<"result">>).
-define(CONTENT_KEY, <<"content">>).

-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(ERROR_NO_LOGIN, <<"用户没有登录">>).
-define(ERROR_ADD_TAG_FAILED, <<"添加标签失败">>).
-define(ERROR_REMOVE_TAG_FAILED, <<"删除标签失败">>).
-define(ERROR_ADD_TAG_FORBIDDED, <<"没有权限添加该标签">>).
-define(ERROR_REMOVE_TAG_FORBIDDED, <<"没有权限删除该标签">>).
-define(ERROR_ADD_TAG_SUCCESS, <<"添加标签成功">>).
-define(ERROR_REMOVE_TAG_SUCCESS, <<"删除标签成功">>).
-define(ERROR_UNKNOWN, <<"未知的错误">>).


handle_get(Req) ->
    {Tag, UrlPageState} = parse_url(Req),
    {PicCount, PicList} = get_tag_pic(Tag),
    {page, PageIndex, PageStatePrev, PageStateNext, PageStateStart} = cal_page(UrlPageState, PicCount),
    ResPicList = woomsg_pic_hook:process_pic_simple_limit({PicCount, PicList}, PageStateStart, ?DEF_PAGE_SIZE),
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            %% 用户登录
            Data = tag_view:index(login, Username, Tag, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
	{logout_remember, undefined} ->
	    %% 用户没登录
	    Data = tag_view:index(logout_remember, ?DEF_USERNAME, Tag, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data});
	{logout_remember, Username} ->
	    %% 用户没登录
	    Data = tag_view:index(logout_remember, Username, Tag, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data});
	{logout_no_remember, undefined} ->
	    %% 用户没登录
	    Data = tag_view:index(logout_no_remember, ?DEF_USERNAME, Tag, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data})
    end.

%% 处理添加/删除评论的Ajax请求:
%% 以JSON的形式返回给客户端数据:
%%
%% <1> 添加评论请求  
%% 请求的URL: 
%% /tag/add/ajax
%%
%% 返回给客户端的数据:
%% {"result": "error",
%%  "content": ?ERROR_XXXX}
%%
%% {"result": "ok",
%%  "content": {"guid":"xxx",
%% 		"tagarray":"xxx"}}   
%% (tagarray是JSON格式的Array)
%%
%% <2> 删除请求 
%% 请求的URL:
%% /tag/remove/ajax
%%
%% 返回给客户端的数据:
%% {"result": "error",
%%  "content": ?ERROR_XXXX}
%%
%% {"result": "ok",
%%  "content": picguid}   
%%

handle_post(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of 
        "tag/add/ajax" ->
	    %% 关注(用户必须首先登录)
	    case woomsg_common:user_state(Req) of
                {login, Username} ->
                    %% 已经登录
		    PostData = Req:parse_post(),
                    case validate_post_data_add(PostData) of
		        {ok, PicGuid, PicTags} ->
			    case add_tags(PicGuid, PicTags, Username) of
				ok ->
				    BinPicTags = lists:foldl(fun(Item, AccIn) ->
							         [ list_to_binary(Item) | AccIn ]
							     end, [], PicTags),
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_OK},
				                               {?CONTENT_KEY, 
								{struct, 
								 [{<<"guid">>, list_to_binary(PicGuid)},
								  {<<"count">>, erlang:length(BinPicTags)},
								  {<<"tags">>, BinPicTags}]}}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				{error, permission_error} ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_ADD_TAG_FORBIDDED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				{error, update_pic_tag_error} ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_ADD_TAG_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				error ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_ADD_TAG_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
			    end;
			{error, unknown} ->
	                    Data = mochijson2:encode({struct,
			   	              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_ADD_TAG_FAILED}]}),
	                    Req:respond({200, [{"Content-Type", "text/plain"}], Data})
                    end;
	        {logout_remember, undefined} ->
	            Data = mochijson2:encode({struct,
				              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_NO_LOGIN}]}),
	            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
	        {logout_remember, _Username} ->
	            Data = mochijson2:encode({struct,
				              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_NO_LOGIN}]}),
	            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
	        {logout_no_remember, undefined} ->
	            Data = mochijson2:encode({struct,
				              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_NO_LOGIN}]}),
	            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
            end;	  
	"tag/remove/ajax"->
	    %% 取消关注(用户必须首先登录)
	    case woomsg_common:user_state(Req) of
                {login, Username} ->
                    %% 已经登录
		    PostData = Req:parse_post(),
	            case validate_post_data_remove(PostData) of
		        {ok, PicGuid, PicTag} ->
			    case del_tag(PicGuid, PicTag, Username) of
				ok ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_OK},
				                               {?CONTENT_KEY, list_to_binary(PicTag)}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				{error, permission_error} ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_TAG_FORBIDDED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				{error, update_pic_tag_error} ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_TAG_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				error ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_TAG_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
			    end;
			{error, unknown} ->
	                    Data = mochijson2:encode({struct,
			   	              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_REMOVE_TAG_FAILED}]}),
	                    Req:respond({200, [{"Content-Type", "text/plain"}], Data})
                    end;
	        {logout_remember, undefined} ->
	            Data = mochijson2:encode({struct,
				              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_NO_LOGIN}]}),
	            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
	        {logout_remember, _Username} ->
	            Data = mochijson2:encode({struct,
				              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_NO_LOGIN}]}),
	            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
	        
	        {logout_no_remember, undefined} ->
	            Data = mochijson2:encode({struct,
				              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_NO_LOGIN}]}),
	            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
            end;	       
	_ ->
	    Data = mochijson2:encode({struct,
				      [{?RESULT_KEY, ?RESULT_ERROR},
				       {?CONTENT_KEY, ?ERROR_UNKNOWN}]}),
	    Req:respond({200, [{"Content-Type", "text/plain"}], Data})
    end.

%% Interlan APIs:

%% 返回tag对应的照片集合:
%% 返回值(和woomsg_pic:get_pic_all/1保持一致的返回值):
%% {0, []}
%% {Count, PicList}
get_tag_pic(Tag) ->
    case woomsg_pic_tag:get_guid_all(Tag) of
	{0, []} ->
	    {0, []};
	{_Count, Guids} when is_list(Guids) ->
	    woomsg_pic:get_pic_all_by_guids(Guids);
	_ ->
	    {0, []}
    end.

%% URL的设计:
%% tag/Tag               
%% tag/Tag/page/N      
%%
%% 解析出URL中的Tag
%% 返回:
%% {[], undefined}
%% {Tag, undefined}  等价于 {Tag, {page, 1}}
%% {Tag, {page, N}}
parse_url(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "tag" ->
	    {[], undefined};
	"tag/" ->
	    {[], undefined};
        _ ->	
            %% <1> 去掉'tag/'前缀
            %% erlang:length("tag/") = 4
            %% tag/Tag/xxx -> Tag/xxx
            PathSuffix = lists:sublist(Path, 5, erlang:length(Path) - 4),
            
            %% <2> 解析出URL中的Tag
            %% Tag/xxx     -> Tag
            %% Tag         -> Tag
            %% Tag?key=val -> Tag?key=val (错误的Tag)
            %%
            %% 注意: 
            %% Tag?key=val这种形式的URL将返回错误的Tag
            %% 我们在设计中避免使用这样的URL
            Tag = woomsg_util:list_index_prefix($/, PathSuffix),
            case Tag =:= PathSuffix of
		true ->
		    {Tag, undefined};
		false ->
		    %% <3> 去掉 tag/Tag/
		    %% tag/Tag/xxx -> xxx	
		    %%     page/N	
		    PathSuffix1 = woomsg_util:list_index_suffix($/, PathSuffix),
		    case woomsg_util:list_index_prefix($/, PathSuffix1) of
			"page" ->
			    NPage = woomsg_util:list_index_suffix($/, PathSuffix1),
			    case woomsg_util:list_to_integer(NPage) of
				undefined ->
				    {Tag, undefined};
				NPageVal ->
				    {Tag, {page, NPageVal}}
			    end;
			 _ ->
			     {Tag, undefined}
                    end
            end
    end.

%% 分页计算:
%% 返回结果:
%% {no_prev, no_next, Start}
%% {no_prev, next, Start}
%% {prev, no_next, Start}
%% {prev, next, Start}
cal_page(undefined, Count) ->
    cal_page({page, 1}, Count);
cal_page({page, Index}, Count) ->
    Start = ?DEF_PAGE_SIZE * (Index - 1) + 1,
    case Start + ?DEF_PAGE_SIZE =< Count of
	true ->
	    case Start =:=1 of
		true ->
		    {page, Index, no_prev, next, Start};
		false ->
		    {page, Index, prev, next, Start}
	    end;
	false ->
	    case Start =:= 1 of
		true ->
		    {page, Index, no_prev, no_next, Start};
		false ->
		    {page, Index, prev, no_next, Start}
	    end
    end.

%% 处理客户端发送来的数据
%% [{"pic-guid", PicGuid},
%%  {"pic-tags", PicTags}]
%%
%% 返回值
%% {error, unknown}
%% {ok, PicGuid, PicTags}
validate_post_data_add([{?PIC_GUID_KEY, PicGuid},
			{?PIC_TAGS_KEY, PicTags}]) ->
    ResPicTags = string:tokens(PicTags, " "),
    {ok, PicGuid, ResPicTags};
validate_post_data_add(_) ->
    {error, unknown}.

%% [{"pic-guid", PicGuid},
%%  {"pic-tag", PicTag}]
%%
%% 返回值
%% {error, unknown}
%% {ok, PicGuid, PicTag}
validate_post_data_remove([{?PIC_GUID_KEY, PicGuid},
			   {?PIC_TAG_KEY, PicTag}]) ->
    {ok, PicGuid, PicTag};
validate_post_data_remove(_) ->
    {error, unknown}.

%% (增加和删除Tag要同时更新pic和pic_tag表格)
%%
%% 增加Tags:
%% 返回值: ok
%%        error
%%        {permission_error}
%%        {error, update_pic_tag_error}
add_tags(PicGuid, TagList, Username) ->
    case woomsg_pic:safe_add_tags(PicGuid, TagList, Username) of
	ok ->
	    case woomsg_pic_tag:add_tags(TagList, PicGuid) of
		ok ->
		    ok;
		error ->
		    {error, update_pic_tag_error}
	    end;
	{error, permission_error} ->
	    {error, permission_error};
	error ->
	    error
    end.

%% 删除一个tag:
%% 返回值: ok
%%        error
%%        {permission_error}
%%        {error, update_pic_tag_error}
del_tag(PicGuid, Tag, Username) ->
    case woomsg_pic:safe_delete_tag(PicGuid, Tag, Username) of
	ok ->
	    case woomsg_pic_tag:remove_tag(Tag, PicGuid) of
		ok ->
		    ok;
		error ->
		    {error, update_pic_tag_error}
	    end;
	{error, permission_error} ->
	    {error, permission_error};
	error ->
	    error
    end.
    
