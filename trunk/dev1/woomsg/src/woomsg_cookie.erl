-module(woomsg_cookie).
-export([get_cookie_of_username/1,
         get_cookie_of_sessionid/1,
         check_sessionid/1, check_sessionid/2]).

-define(SES_USERNAME, "ses_username").
-define(SES_SESSION_ID, "ses_session_id").

%% Usage:
%% CookieUsr = get_cookie_of_username(Username),
%% CookieSid = get_cookie_of_sessionid(Username),
%% Req:respond({200, [{"Content-Type", "text/html"}, CookieUsr, CookieSid], Data}).

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

%% 检查Req中的session_id是否和数据库中的匹配, 如果匹配，表示
%% 用户登录.
%% 匹配返回: true
%% 不匹配返回: false
check_sessionid(Req) ->
    Username = Req:get_cookie_val(?SES_USERNAME),
    check_sessionid(Req, Username).

check_sessionid(Req, Username) ->
    Sid = Req:get_cookie_val(?SES_SESSION_ID),
    case woomsg_session:get_sessionid(Username) of
	[] ->
	    false;
         Val ->
	    Sid =:= Val
    end.
