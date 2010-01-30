-module(pic_controller).
-export([handle_get/1, handle_post/1]).

%% 客户端POST的数据Keys
-define(PIC_GUID_KEY, "pic-guid").

%% JSON - 返回值
-define(RESULT_KEY, <<"result">>).
-define(CONTENT_KEY, <<"content">>).

-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(ERROR_NO_LOGIN, <<"用户没有登录">>).
-define(ERROR_REMOVE_PIC_FORBIDDED, <<"没有权限删除该照片">>).
-define(ERROR_REMOVE_PIC_FAILED, <<"删除照片失败">>).
-define(ERROR_UNKNOWN, <<"未知的错误">>).

handle_get(_Req) ->
    ok.

%% 处理删除图片的Ajax请求:
%% 以JSON的形式返回客户端数据:
%%
%% <1> 删除照片
%% 请求URL:
%% /pic/remove/ajax
%%
%% 返回给客户端的数据:
%% {"result": "error",
%%  "content": ?ERROR_XXXX}
%%
%% {"result": "ok",
%%  "content": PicGuid}   
%%
handle_post(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
	"pic/remove/ajax" ->
            %% 删除照片(用户必须首先登录)
            case woomsg_common:user_state(Req) of
		{login, Username} ->
		    %% 已经登录
		    PostData = Req:parse_post(),
		    case validate_post_data_remove(PostData) of
			{ok, PicGuid} ->
			    %% 删除照片的核心逻辑
			    %% (删除权限 - 只有照片的拥有者可以删除该照片)
			    case woomsg_image:safe_delete_pic(PicGuid, Username) of
				ok ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_OK},
				                               {?CONTENT_KEY, list_to_binary(PicGuid)}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				{error, permission_error} ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_PIC_FORBIDDED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				error ->
				    Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_PIC_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
			    end;
			{error, unknown} ->
			    Data = mochijson2:encode({struct,
			   	              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_REMOVE_PIC_FAILED}]}),
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

%% [{"pic-guid", PicGuid}]
%%
%% 返回值
%% {error, unknown}
%% {ok, PicGuid}
validate_post_data_remove([{?PIC_GUID_KEY, PicGuid}]) ->
    {ok, PicGuid};
validate_post_data_remove(_) ->
    {error, unknown}.
    
