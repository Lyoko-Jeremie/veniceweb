-module(woomsg_session).
-export([set_logout/1,
         set_session/2,
	 get_sessionid/1]).

-define(SID_LOGOUT, "logout").

set_logout(Username) ->
    set_session(Username, ?SID_LOGOUT).

%% 插入一条新的记录(或者更新)session表.
%% 成功返回: ok
%% 失败返回: error
set_session(Username, SessionId) ->
    F = fun() ->
	    mnesia:write({session, Username, SessionId})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 获取当前的session_id
%% 成功返回: SessionId
%% 失败返回: []
get_sessionid(Username) ->
    F = fun() ->
	    mnesia:read({session, Username})
	end,
    case mnesia:transaction(F) of
	{atomic, [{session, Username, SessionId}]} ->
	    SessionId;
	_ ->
	    []
    end.

