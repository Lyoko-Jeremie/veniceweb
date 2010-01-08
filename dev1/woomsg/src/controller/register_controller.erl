-module(register_controller).
-export([handle_get/1, handle_post/1]).

%% 客户端POST的数据
-define(USERNAME_KEY, "username").
-define(PASSWORD_KEY, "password").
-define(PASSWORD_AGAIN_KEY, "password_again").
-define(EMAIL_KEY, "email").

%% JSON - 返回价值
-define(RESULT_KEY, <<"result">>).
-define(CONTENT_KEY, <<"content">>).

-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(ERROR_NULL_USERNAME, <<"用户名不能为空">>).
-define(ERROR_NULL_PASSWORD, <<"密码不能为空">>).
-define(ERROR_NULL_EMAIL, <<"邮箱地址不能为空">>).
-define(ERROR_NO_MATCH_PASSWORD, <<"两次输入的密码不匹配, 请重新输入">>).
-define(ERROR_REGISTERED, <<"用户名已经被注册">>).
-define(ERROR_REGISTER_FAILED, <<"注册失败">>).
-define(ERROR_UNKNOWN, <<"未知的错误">>).


handle_get(Req) ->
    Data = register_view:index(register_page, undefined),
    Req:respond({200, [{"Content-Type", "text/html"}], Data}).

%% 处理用户注册的Ajax请求
%% 以JSON的形式返回给客户端数据
%%
%% 格式:
%% <1> 注册失败
%% {"result":"error",
%%  "content":ERROR_XXXX}
%%
%% <2> 注册成功
%% {"result":"ok",
%%  "content":Username}
%% 注意: 注册成功会同时更新客户端的Cookies
handle_post(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "register/ajax" ->
	    PostData = Req:parse_post(),
            case validate_post_data(PostData) of
		{error, null_username} ->
	            Data = mochijson2:encode({struct, 
					      [{?RESULT_KEY, ?RESULT_ERROR},
					       {?CONTENT_KEY, ?ERROR_NULL_USERNAME}]}),
                    Req:respond({200, [{"Content-Type", "text/plain"}], Data});
		{error, null_password} ->
	            Data =  mochijson2:encode({struct, 
					       [{?RESULT_KEY, ?RESULT_ERROR},
						{?CONTENT_KEY, ?ERROR_NULL_PASSWORD}]}),
                    Req:respond({200, [{"Content-Type", "text/plain"}], Data});
		{error, null_email} ->
		    Data = mochijson2:encode({struct, 
					      [{?RESULT_KEY, ?RESULT_ERROR},
					       {?CONTENT_KEY, ?ERROR_NULL_EMAIL}]}),
                    Req:respond({200, [{"Content-Type", "text/plain"}], Data});
		{error, no_match_password} ->
		    Data =  mochijson2:encode({struct, 
					       [{?RESULT_KEY, ?RESULT_ERROR},
					        {?CONTENT_KEY, ?ERROR_NO_MATCH_PASSWORD}]}),
                    Req:respond({200, [{"Content-Type", "text/plain"}], Data});
		{ok, Username, Password, Email} ->
	            %% 用户的注册逻辑
	            case woomsg_register:is_registered(Username) of
		        true ->
			    Data = mochijson2:encode({struct, 
				        	      [{?RESULT_KEY, ?RESULT_ERROR},
						       {?CONTENT_KEY, ?ERROR_REGISTERED}]}),
                            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
			false ->
		            case woomsg_register:register(Username, Password, Email) of
			        ok ->
			            %% 注册成功, 让客户端javascript跳转到登录页面:
				    %% 这里使用了一个小技巧:
				    %% <1> 设置cookie, remember=true
				    %% <2> 然后设置Username logout, 
				    %% 这样跳转到login页面的时候, 会通过cookie中的Username来显示用户名.
                                    CookieUsr = woomsg_cookie:get_cookie_of_username(Username),
				    CookieSid = woomsg_cookie:get_cookie_of_sessionid(Username),
				    CookieRem = woomsg_cookie:get_cookie_of_remember(true),
				    woomsg_session:set_logout(Username),
			            Data = mochijson2:encode({struct, 
				        		      [{?RESULT_KEY, ?RESULT_OK},
						               {?CONTENT_KEY, list_to_binary(Username)}]}),
                                    Req:respond({200, [{"Content-Type", "text/plain"}, CookieUsr, CookieSid, CookieRem], Data});
				error ->
			            Data = mochijson2:encode({struct, 
				        		      [{?RESULT_KEY, ?RESULT_ERROR},
						               {?CONTENT_KEY, ?ERROR_REGISTER_FAILED}]}),
                                    Req:respond({200, [{"Content-Type", "text/plain"}], Data})
		             end
		     end;
	         _ ->
	             mochijson2:encode({struct, 
						[{?RESULT_KEY, ?RESULT_ERROR},
						 {?CONTENT_KEY, ?ERROR_UNKNOWN}]})
            end;
	_ ->
	    Data = mochijson2:encode({struct, 
			              [{?RESULT_KEY, ?RESULT_ERROR},
				       {?CONTENT_KEY, ?ERROR_UNKNOWN}]}),
            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
    end.


%% Interlan APIs:

%% 处理客户端发送来的数据:
%% 
%% [{"username", Username},
%%  {"password", Password},
%%  {"password_again", PasswordAgain},
%%  {"email", Email}]
%%  这五个值可能为[].
%%
%% 返回值:
%% {error, null_username}
%% {error, null_password}
%% {error, null_email}
%% {error, no_match_password}
%% {error, unknown}
%% {ok, Username, Password, Email}
validate_post_data([{?USERNAME_KEY, Username},
                    {?PASSWORD_KEY, Password},
                    {?PASSWORD_AGAIN_KEY, PasswordAgain},
                    {?EMAIL_KEY, Email}]) ->
    case Username =:= [] of
	true ->
            {error, null_username};
	false ->
            case Password =:= [] of
		true ->
	            {error, null_password};
		false ->
		    case Password =:= PasswordAgain of
	                true ->
		             case Email =:= [] of
			         true ->
			             {error, null_email};
			         false ->
			             {ok, Username, Password, Email}
			     end;
  			 false ->
			     {error, no_match_password}
		     end
            end
    end;
validate_post_data(_) ->
    {error, unknown}.


