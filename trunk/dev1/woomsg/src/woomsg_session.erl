-module(woomsg_session).
-export([set/1, get/1]).


%% 成功返回: {atomic, ok}
set(Key) ->
    Val = woomsg_util:get_guid(),
    F = fun() ->
		mnesia:write({session, Key, Val})
	end,
    mnesia:transaction(F).


%% 成功返回: {atomic, Val}
%% 失败返回: {atomic,{error,key_not_found}}
get(Key) ->
    db:get(session, Key, session_id).







