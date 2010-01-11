-module(following_controller).
-export([handle_get/1, handle_post/1]).

%% 客户端POST的数据Keys
-define(USERNAME_KEY, "username").

%% JSON - 返回价值
-define(RESULT_KEY, <<"result">>).
-define(CONTENT_KEY, <<"content">>).

-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(ERROR_NO_LOGIN, <<"用户没有登录">>).
-define(ERROR_ADD_FOLLOWING_FAILED, <<"添加关注失败">>).
-define(ERROR_REMOVE_FOLLOWING_FAILED, <<"删除关注失败">>).
-define(ERROR_UNKNOWN, <<"未知的错误">>).


handle_get(_Req) ->
    ok.

%% 处理添加/删除关注的Ajax请求:
%% 以JSON的形式返回给客户端数据:
%%
%% <1> 添加请求  
%% 请求的URL: 
%% /following/add/ajax
%%
%% 返回给客户端的数据:
%% {"result": "error",
%%  "content": ?ERROR_XXXX}
%%
%% {"result": "ok",
%%  "content": FUsername}   
%%
%% <2> 删除请求 
%% 请求的URL:
%% /following/remove/ajax
%%
%% 返回给客户端的数据:
%% {"result": "error",
%%  "content": ?ERROR_XXXX}
%%
%% {"result": "ok",
%%  "content": FUsername}   
%%
handle_post(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of 
        "following/add/ajax" ->
	    %% 关注(用户必须首先登录)
	    case woomsg_common:user_state(Req) of
                {login, Username} ->
                    %% 已经登录
		    PostData = Req:parse_post(),
                    case validate_post_data(PostData) of
		        {ok, FUsername} ->
			    case woomsg_following:add_following(Username, FUsername) of
			        ok ->
	                            Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_OK},
				                               {?CONTENT_KEY, list_to_binary(FUsername)}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				error ->
	                            Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_ADD_FOLLOWING_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
                            end;
			{error, unknown} ->
	                    Data = mochijson2:encode({struct,
			   	              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_ADD_FOLLOWING_FAILED}]}),
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
	"following/remove/ajax"->
	    %% 取消关注(用户必须首先登录)
	    case woomsg_common:user_state(Req) of
                {login, Username} ->
                    %% 已经登录
		    PostData = Req:parse_post(),
	            case validate_post_data(PostData) of
		        {ok, FUsername} ->
			    case woomsg_following:remove_following(Username, FUsername) of
			        ok ->
	                            Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_OK},
				                               {?CONTENT_KEY, list_to_binary(FUsername)}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
				error ->
	                            Data = mochijson2:encode({struct,
			   	                              [{?RESULT_KEY, ?RESULT_ERROR},
				                               {?CONTENT_KEY, ?ERROR_REMOVE_FOLLOWING_FAILED}]}),
	                            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
                            end;
			{error, unknown} ->
	                    Data = mochijson2:encode({struct,
			   	              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_REMOVE_FOLLOWING_FAILED}]}),
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
%% [{"username", FUsername}]
%%
%% 返回值
%% {error, unknown}
%% {ok, FUsername}
validate_post_data([{?USERNAME_KEY, FUsername}]) ->
    {ok, FUsername};
validate_post_data(_) ->
    {error, unknown}.
