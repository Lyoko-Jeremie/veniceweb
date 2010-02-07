-module(index_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).
-define(DEF_PIC_SIZE, 10).

handle_get(Req) ->
    Data = case woomsg_common:user_state(Req) of
	       {login, Username} ->
                   %% 用户已经登录, 跳转到用户主页
                   Req:respond({302, [{"Location", "/user/" ++ Username}], []});
	       {logout_remember, undefined} ->
		   ResPicList = get_pic(),
		   index_view:index(logout_remember, ?DEF_USERNAME, ResPicList);
	       {logout_remember, Username} ->
		   ResPicList = get_pic(),
	           index_view:index(logout_remember, Username, ResPicList);
	       {logout_no_remember, undefined} ->
		   ResPicList = get_pic(),
	           index_view:index(logout_no_remember, ?DEF_USERNAME, ResPicList)
           end,
    Req:respond({200, [{"Content-Type","text/html"}], Data}).

handle_post(_Req) ->
    ok.

%% 返回值(和woomsg_pic:get_pic_all/1保持一致的返回值):
%% {0, []}
%% {Count, PicList}
%%
%% TODO:
%% 由于首页的刷新频率比较高, 这部分图片可以cache出来, 定期更新.
%%
get_pic() ->
    case woomsg_pic:get_all() of
	{0, []} ->
	    {0, []};
	{PicCount, PicList} when is_list(PicList) ->
	    woomsg_pic_hook:process_pic_simple_limit({PicCount, PicList}, ?DEF_PIC_SIZE);
	_ ->
	    {0, []}
    end.


    



