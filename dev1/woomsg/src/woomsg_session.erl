-module(woomsg_session).
-export([get_cookie_session_id/1, get_cookie_username/1, 
	 set_logout/1, set_session_id/2,
	 get_session_id/1,
	 check_session_id/1, check_session_id/2]).

-define(SID_LOGOUT, "logout").

%% Usage:
%% CookieSid = get_cookie_session_id(Username),
%% CookieUsr = get_cookie_username(Username),
%% Req:respond({200, [{"Content-Type", "text/html"}, CookieSid, CookieUsr], Data})
%% 

%% 产生发送给客户段的cookie/并更新数据库
get_cookie_session_id(Username) ->
    Val = woomsg_util:get_guid(),
    F = fun() ->
		mnesia:write({session, Username, Val})
	end,
    mnesia:transaction(F),
    mochiweb_cookies:cookie("ses_session_id", Val, []).

%% 产生发送给客户段的cookie
get_cookie_username(Username) ->
    mochiweb_cookies:cookie("ses_username", Username, []).

set_logout(Username) ->
    set_session_id(Username, ?SID_LOGOUT).

%% 成功返回: {atomic, ok}
set_session_id(Username, SessionId) ->
    F = fun() ->
		mnesia:write({session, Username, SessionId})
	end,
    mnesia:transaction(F).

%% 获取当前的session_id
%% 成功返回: SessionId
%% 失败返回: []
get_session_id(Username) ->
    case db:find(session, Username) of
	{atomic, [{session, Username, SessionId}]} ->
	    SessionId;
	_ ->
	    []
    end.

%% 将查Req中的session_id是否于数据库中的匹配, 如果匹配, 
%% 表示用户登录.
check_session_id(Req) ->
    Username = Req:get_cookie_val("ses_username"),
    check_session_id(Req, Username).

check_session_id(Req, Username) ->
    Sid = Req:get_cookie_val("ses_session_id"),
    case woomsg_session:get_session_id(Username) of
	[] ->
	    false;
	Val ->
	    Sid =:= Val	    
    end.
    







