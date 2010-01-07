-module(login_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).

-define(USERNAME_KEY, "username").
-define(PASSWORD_KEY, "password").
-define(REMEMBER_KEY, "remember").

-define(REMEMBER_VAL, "1").

%% JSON - 返回值
-define(RESULT_KEY, <<"result">>).
-define(CONTENT_KEY, <<"content">>).

-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(ERROR_NULL_USERNAME, <<"用户名不能为空">>).
-define(ERROR_NULL_PASSWORD, <<"密码不能为空">>).
-define(ERROR_LOGIN_FAILED, <<"登录失败">>).
-define(ERROR_UNKNOWN, <<"未知的错误">>).

-define(LOGIN_SUCCESS, <<"登录成功">>).

handle_get(Req) ->
    Data = login_view:index(login_page, undefined),
    Req:respond({200, [{"Content-Type", "text/html"}], Data}).

handle_post(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "login/ajax" ->
	    PostData = Req:parse_post(),
	    io:format("PostData: ~p~n", [PostData]),
            case validate_post_data(PostData) of
                {error, null_username} ->
	            Data = mochijson2:encode({struct,
                                              [{?RESULT_KEY, ?RESULT_ERROR},
			        	       {?CONTENT_KEY, ?ERROR_NULL_USERNAME}]}),
		    Req:respond({200, [{"Content-Type", "text/plain"}], Data});
                {error, null_password} ->
	            Data = mochijson2:encode({struct,
                                              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_NULL_PASSWORD}]}),
		    Req:respond({200, [{"Content-Type", "text/plain"}], Data});
                {ok, Username, Password, BoolRemember} ->
                    %% 验证用户名和密码(成功返回true, 失败返回false)
                    case woomsg_login:check_password(Username, Password) of
	                true ->
                            %% 验证成功
                            %% <1> 根据新的参数username/password/remember更新cookie
                            %% <2> 跳转到用户Username主页
                            CookieUsr = woomsg_cookie:get_cookie_of_username(Username),
                            CookieSid = woomsg_cookie:get_cookie_of_sessionid(Username),
                            CookieRem = woomsg_cookie:get_cookie_of_remember(BoolRemember),
	                    Data = mochijson2:encode({struct,
                                                      [{?RESULT_KEY, ?RESULT_OK},
				                       {?CONTENT_KEY, list_to_binary(Username)}]}),
                            Req:respond({200, [{"Content-Type", "text/plain"}, CookieUsr, CookieSid, CookieRem], Data});
			false ->
	                    Data = mochijson2:encode({struct,
                                                      [{?RESULT_KEY, ?RESULT_ERROR},
			                	       {?CONTENT_KEY, ?ERROR_LOGIN_FAILED}]}),
		            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
		    end;
		{error, unknown} ->
	            Data = mochijson2:encode({struct,
                                              [{?RESULT_KEY, ?RESULT_ERROR},
				               {?CONTENT_KEY, ?ERROR_UNKNOWN}]}),
		    Req:respond({200, [{"Content-Type", "text/plain"}], Data})
            end;
        _ ->
	    Data = mochijson2:encode({struct,
                                      [{?RESULT_KEY, ?RESULT_ERROR},
				       {?CONTENT_KEY, ?ERROR_UNKNOWN}]}),
            Req:respond({200, [{"Content-Type", "text/plain"}], Data})
    end.
  
%% Internal APIs:
  
%% [{"username", Username}, 
%%  {"password", Password}]
%% 或者
%% [{"username", Username},
%%  {"password", Password},
%%  {"remember", "1"}]
%%
%% 返回值
%% {error, null_username}
%% {error, null_password}
%% {error, unknown}
%% {ok, Username, Password, Remember} Remember = true | false
%%
validate_post_data([{?USERNAME_KEY, Username},
                    {?PASSWORD_KEY, Password},
                    {?REMEMBER_KEY, ?REMEMBER_VAL}]) ->
    case Username =:= [] of
	true ->
	    {error, null_username};
	false ->
	    case Password =:= [] of
 	        true ->
		    {error, null_password};
		false ->
		    {ok, Username, Password, true}
	    end
    end;  
validate_post_data([{?USERNAME_KEY, Username}, 
                    {?PASSWORD_KEY, Password}]) ->
    case Username =:= [] of
	true ->
	    {error, null_username};
	false ->
	    case Password =:= [] of
		true ->
		    {error, null_password};
		false ->
		    {ok, Username, Password, false}
  	    end
    end;
validate_post_data(_) ->
    {error, unknown}.
