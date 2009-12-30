-module(index_controller).
-export([handle_get/2, handle_post/2]).

-define(DEF_USERNAME, <<"请输入用户名">>).

handle_get(Req, _DocRoot) ->
    Data = case woomsg_common:user_state(Req) of
	       {login, Username} ->
	           index_view:index(login, Username);
	       {logout_remember, undefined} ->
		   index_view:index(logout_remember, ?DEF_USERNAME);
	       {logout_remember, Username} ->
	           index_view:index(logout_remember, Username);
	       {logout_no_remember, undefined} ->
	           index_view:index(logout_no_remember, ?DEF_USERNAME)
           end,
    Req:respond({200, [{"Content-Type","text/html"}], Data}).

handle_post(_Req, _DocRoot) ->
    ok.


    



