-module(publictimeline_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).

-define(DEF_PAGE_SIZE, 25).

handle_get(Req) ->
    UrlPageState = parse_url(Req),
    case woomsg_common:user_state(Req) of
        {login, Username} ->
	    render_page(Req, login, Username, UrlPageState);
        {logout_remember, undefined} ->
            render_page(Req, logout_remember, ?DEF_USERNAME, UrlPageState);
        {logout_remember, Username} ->
            render_page(Req, logout_remember, Username, UrlPageState);
        {logout_no_remember, undefined} ->
            render_page(Req, logout_no_remember, ?DEF_USERNAME, UrlPageState)
    end.

handle_post(_Req) ->
    ok.

%% Internal APIs:
%%

render_page(Req, login, Username, UrlPageState) ->
    {PicCount, PicList} = woomsg_pic:get_all(),
    {page, PageIndex, PageStatePrev, PageStateNext, PageStateStart} = cal_page(UrlPageState, PicCount),
    ResPicList = woomsg_pic_hook:process_pic_limit_with_user_photo({PicCount, PicList}, Username, PageStateStart, ?DEF_PAGE_SIZE),
    Data = publictimeline_view:index(login, Username, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
    Req:respond({200, [{"Content-Type","text/html"}], Data});
render_page(Req, LogoutState, Username, UrlPageState) ->
    {PicCount, PicList} = woomsg_pic:get_all(),
    {page, PageIndex, PageStatePrev, PageStateNext, PageStateStart} = cal_page(UrlPageState, PicCount),
    ResPicList = woomsg_pic_hook:process_pic_limit_with_user_photo({PicCount, PicList}, undefined, PageStateStart, ?DEF_PAGE_SIZE),
    Data = publictimeline_view:index(LogoutState, Username, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
    Req:respond({200, [{"Content-Type","text/html"}], Data}).

%% 分页计算:
%% 返回结果:
%% {no_prev, no_next, Start}
%% {no_prev, next, Start}
%% {prev, no_next, Start}
%% {prev, next, Start}
cal_page({page, Index}, Count) ->
    Start = ?DEF_PAGE_SIZE * (Index - 1) + 1,
    case Start + ?DEF_PAGE_SIZE =< Count of
	true ->
	    case Start =:=1 of
		true ->
		    {page, Index, no_prev, next, Start};
		false ->
		    {page, Index, prev, next, Start}
	    end;
	false ->
	    case Start =:= 1 of
		true ->
		    {page, Index, no_prev, no_next, Start};
		false ->
		    {page, Index, prev, no_next, Start}
	    end
    end.

%% 返回
%% {page, NPage:integer()}
parse_url(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
	"publictimeline" ->
	    {page, 1};
	"publictimeline/" ->
	    {page, 1};
	_ ->
	    %% <1> 去掉'publictimeline/'前缀
            %% erlang:length("publictimeline/") = 15
            %% publictimeline/xxx -> xxx
            PathSuffix = lists:sublist(Path, 16, erlang:length(Path) - 15),
            case woomsg_util:list_to_integer(PathSuffix) of
		undefined ->
		    {page, 1};
		NPage ->
		    {page, NPage}
            end
    end.

