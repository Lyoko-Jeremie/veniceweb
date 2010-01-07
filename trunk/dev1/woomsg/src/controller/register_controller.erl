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
-define(ERROR_NO_MATCH_PASSWORD, <<"两次输入的密码不匹配">>).
-define(ERROR_REGISTERED, <<"用户名已经被注册">>).
-define(ERROR_REGISTER_FAILED, <<"注册失败">>).
-define(ERROR_UNKNOWN, <<"未知的错误">>).

-define(REGISTER_SUCCESS, <<"注册成功">>).

handle_get(Req) ->
    Data = register_view:index(register_page, undefined),
    Req:respond({200, [{"Content-Type", "text/html"}], Data}).

handle_post(Req) ->
    "/" ++ Path = Req:get(path),
    Data = case Path of
               "register/ajax" ->
	           PostData = Req:parse_post(),
                   case validate_post_data(PostData) of
		       {error, null_username} ->
			    mochijson2:encode({struct, 
						[{?RESULT_KEY, ?RESULT_ERROR},
						 {?CONTENT_KEY, ?ERROR_NULL_USERNAME}]});
		       {error, null_password} ->
			    mochijson2:encode({struct, 
						[{?RESULT_KEY, ?RESULT_ERROR},
						 {?CONTENT_KEY, ?ERROR_NULL_PASSWORD}]});
		       {error, null_email} ->
			    mochijson2:encode({struct, 
						[{?RESULT_KEY, ?RESULT_ERROR},
						 {?CONTENT_KEY, ?ERROR_NULL_EMAIL}]});
		       {error, no_match_password} ->
			    mochijson2:encode({struct, 
						[{?RESULT_KEY, ?RESULT_ERROR},
						 {?CONTENT_KEY, ?ERROR_NO_MATCH_PASSWORD}]});
		       {ok, Username, Password, Email} ->
			    %% 用户的注册逻辑
			    case woomsg_register:is_registered(Username) of
			        true ->
			            mochijson2:encode({struct, 
				        		[{?RESULT_KEY, ?RESULT_ERROR},
						         {?CONTENT_KEY, ?ERROR_REGISTERED}]});
				false ->
				    case woomsg_register:register(Username, Password, Email) of
					ok ->
					    %% 注册成功, 跳转到登录页面:
			                    mochijson2:encode({struct, 
				        		        [{?RESULT_KEY, ?RESULT_OK},
						                 {?CONTENT_KEY, ?REGISTER_SUCCESS}]});
					error ->
			                    mochijson2:encode({struct, 
				        		        [{?RESULT_KEY, ?RESULT_ERROR},
						                 {?CONTENT_KEY, ?ERROR_REGISTER_FAILED}]})
				    end
			    end;
			_ ->
			    mochijson2:encode({struct, 
						[{?RESULT_KEY, ?RESULT_ERROR},
						 {?CONTENT_KEY, ?ERROR_UNKNOWN}]})
                   end;
	       _ ->
	           mochijson2:encode({struct, 
				      [{?RESULT_KEY, ?RESULT_ERROR},
				       {?CONTENT_KEY, ?ERROR_UNKNOWN}]})
           end,
    Req:respond({200, [{"Content-Type", "text/plain"}], Data}).


%% Interlan APIs:

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


