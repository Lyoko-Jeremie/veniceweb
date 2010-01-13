-module(comment_controller).
-export([handle_get/1, handle_post/1]).

%% 客户端POST的数据Keys
-define(PIC_GUID_KEY, "pic-guid").
-define(COMMENT_GUID_KEY, "comment-guid").
-define(PIC_COMMENT_KEY, "pic-comment").


%% JSON - 返回价值
-define(RESULT_KEY, <<"result">>).
-define(CONTENT_KEY, <<"content">>).

-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(ERROR_NO_LOGIN, <<"用户没有登录">>).
-define(ERROR_ADD_COMMENT_FAILED, <<"添加评论失败">>).
-define(ERROR_REMOVE_COMMENT_FAILED, <<"删除评论失败">>).
-define(ERROR_REMOVE_COMMENT_FORBIDDED, <<"没有权限删除该评论">>).
-define(ERROR_ADD_COMMENT_SUCCESS, <<"添加评论成功">>).
-define(ERROR_REMOVE_COMMENT_SUCCESS, <<"删除评论成功">>).
-define(ERROR_UNKNOWN, <<"未知的错误">>).


handle_get(_Req) ->
    ok.

%% 处理添加/删除评论的Ajax请求:
%% 以JSON的形式返回给客户端数据:
%%
%% <1> 添加评论请求  
%% 请求的URL: 
%% /comment/add/ajax
%%
%% 返回给客户端的数据:
%% {"result": "error",
%%  "content": ?ERROR_XXXX}
%%
%% {"result": "ok",
%%  "content": {"guid":"xxx",
%% 		"username":"xxx",
%%		"comment":"xxx",
%%		"photosrc":"xxx"
%%		"createdate":"xxx"}}   
%%
%% <2> 删除请求 
%% 请求的URL:
%% /comment/remove/ajax
%%
%% 返回给客户端的数据:
%% {"result": "error",
%%  "content": ?ERROR_XXXX}
%%
%% {"result": "ok",
%%  "content": CommentGuid}   
%%
handle_post(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of 
        "comment/add/ajax" ->
	    %% 关注(用户必须首先登录)
	    case woomsg_common:user_state(Req) of
                {login, Username} ->
                    %% 已经登录
		    PostData = Req:parse_post(),
                    case validate_post_data_add(PostData) of
		        {ok, PicGuid, PicComment} ->
			    %% 添加评论的核心逻辑
			    %%
			    CommentGuid = woomsg_guid:get_comment_guid(),
                            CommentCreateDate = woomsg_datetime:get_datetime(),
			    
			    case woomsg_pic_comment:new_pic_comment(CommentGuid, PicGuid, Username, PicComment, CommentCreateDate) of
				ok ->
				    {Username, _Pwd, _Email, UPhotoGuid, UPhotoPath, UPhotoType} = woomsg_user:get_user(Username),
				    FmtCommentCreateDate = woomsg_datetime:get_fmt_since_datetime_string(CommentCreateDate),
				    PhotoSrc = woomsg_image:get_image_path(UPhotoPath, UPhotoGuid, UPhotoType, "mini"),
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_OK},
				                               {?CONTENT_KEY, 
                                                                 {struct, 
                                                                  [{<<"guid">>, list_to_binary(CommentGuid)},
								   {<<"username">>, list_to_binary(Username)},
								   {<<"comment">>, list_to_binary(PicComment)},
								   {<<"photosrc">>, list_to_binary(PhotoSrc)},
								   {<<"createdate">>, list_to_binary(FmtCommentCreateDate)}]}}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				error ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_ADD_COMMENT_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
                            end;
			{error, unknown} ->
	                    Data = mochijson2:encode({struct,
			   	              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_ADD_COMMENT_FAILED}]}),
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
	"comment/remove/ajax"->
	    %% 取消关注(用户必须首先登录)
	    case woomsg_common:user_state(Req) of
                {login, Username} ->
                    %% 已经登录
		    PostData = Req:parse_post(),
	            case validate_post_data_remove(PostData) of
		        {ok, CommentGuid} ->
			    %% 删除评论的核心逻辑
			    %% (删除权限 - 只有评论的拥有者可以删除该评论)
			    case woomsg_pic_comment:safe_delete_pic_comment(CommentGuid, Username) of
				ok ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_OK},
				                               {?CONTENT_KEY, list_to_binary(CommentGuid)}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				{error, permission_error} ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_COMMENT_FORBIDDED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				error ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_COMMENT_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
			    end;
			{error, unknown} ->
	                    Data = mochijson2:encode({struct,
			   	              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_REMOVE_COMMENT_FAILED}]}),
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
    
%% Internal APIs:
%%

%% 处理客户端发送来的数据
%% [{"pic-guid", PicGuid},
%%  {"pic-comment", PicComment}]
%%
%% 返回值
%% {error, unknown}
%% {ok, PicGuid, PicComment}
validate_post_data_add([{?PIC_GUID_KEY, PicGuid},
		    {?PIC_COMMENT_KEY, PicComment}]) ->
    {ok, PicGuid, PicComment};
validate_post_data_add(_) ->
    {error, unknown}.

%% [{"comment-guid", CommentGuid}]
%%
%% 返回值
%% {error, unknown}
%% {ok, CommentGuid}
validate_post_data_remove([{?COMMENT_GUID_KEY, CommentGuid}]) ->
    {ok, CommentGuid};
validate_post_data_remove(_) ->
    {error, unknown}.

