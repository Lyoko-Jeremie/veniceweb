-module(woomsg_cookie).
-export([get_cookie_of_remember/1,
         get_cookie_of_username/1,
         get_cookie_of_sessionid/1,
         parse_cookie_remember/1,
         parse_cookie_username/1,
         parse_cookie_sessionid/1,
         check_session_remember/1,
         check_session_login/1, check_session_login/2]).

-define(SES_USERNAME, "ses_username").
-define(SES_SESSION_ID, "ses_session_id").
-define(SES_REMEMBER, "ses_remember").

-define(SES_REMEMBER_TRUE, "remember_true").
-define(SES_REMEMBER_FALSE, "remember_false").

%% Usage:
%% 注意:
%% 如果返回的Cookie是undefined, 不能直接显示, 需要转换成string或者binary
%% CookieUsr = get_cookie_of_username(Username),
%% CookieSid = get_cookie_of_sessionid(Username),
%% Req:respond({200, [{"Content-Type", "text/html"}, CookieUsr, CookieSid], Data}).

%%
%% 产生发送给客户端的Cookie: - ses_remember
%% 参数: true | false
get_cookie_of_remember(State) when State ->
    mochiweb_cookies:cookie(?SES_REMEMBER, ?SES_REMEMBER_TRUE, []);
get_cookie_of_remember(_State) ->
    mochiweb_cookies:cookie(?SES_REMEMBER, ?SES_REMEMBER_FALSE, []).


%% 产生发送给客户端的Cookie: - ses_username
get_cookie_of_username(Username) ->
    mochiweb_cookies:cookie(?SES_USERNAME, Username, []).

%% 产生发送给客户端的cookie: - ses_session_id, 并更新session表
%% 成功返回: cookie
%% 失败返回: []
get_cookie_of_sessionid(Username) ->
    SessionId = woomsg_guid:get_session_guid(),
    case woomsg_session:set_session(Username, SessionId) of
        ok ->
	    mochiweb_cookies:cookie(?SES_SESSION_ID, SessionId, []);
	_ ->
	    []
    end.

%% 存在返回Val:string(), 不存在返回undefined.
parse_cookie_remember(Req) ->
    Req:get_cookie_value(?SES_USERNAME).

%% 存在返回Val:string(), 不存在返回undefined.
parse_cookie_username(Req) ->
    Req:get_cookie_value(?SES_USERNAME).

%% 存在返回Val:string(), 不存在返回undefined.
parse_cookie_sessionid(Req) ->
    Req:get_cookie_value(?SES_USERNAME).

%% 检查Cookie中是否保存了ses_remember = remember_true,
%%  如果保存了, 则返回true, 否则
%% 返回false.
check_session_remember(Req) ->
    Req:get_cookie_value(?SES_REMEMBER) =:= ?SES_REMEMBER_TRUE.

%% 检查Req中Cookie的session_id是否和数据库中的匹配, 如果匹配，表示
%% 用户登录.
%% 匹配返回: true
%% 不匹配返回: false
check_session_login(Req) ->
    %% Username:string() | undefined
    case Req:get_cookie_value(?SES_USERNAME) of
        undefined ->
	    false;
	Username ->
            check_session_login(Req, Username)
    end.

check_session_login(Req, Username) ->
    case Req:get_cookie_value(?SES_SESSION_ID) of
        undefined ->
	    false;
	Sid ->
            case woomsg_session:get_sessionid(Username) of
 	        [] ->
	            false;
                Val ->
	            Sid =:= Val
            end
    end.
