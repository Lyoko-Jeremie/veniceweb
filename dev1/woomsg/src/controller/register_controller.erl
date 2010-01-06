-module(register_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).

handle_get(Req) ->
    case woomsg_common:user_state(Req) of
	{login, Username} ->
	    Data = register_view:index(login, Username),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data});
	{logout_remember, undefined} ->
	    Data = register_view:index(login_remember, ?DEF_USERNAME),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data});
	{logout_remember, Username} ->
	    Data = register_view:index(logout_remember, Username),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data});
	{logout_no_remember, undefined} ->
	    Data = register_view:index(logout_no_remember, ?DEF_USERNAME),
	    Req:respond({200, [{"Content-Type", "text/html"}], Data})
    end.

handle_post(_Req) ->
    ok.


