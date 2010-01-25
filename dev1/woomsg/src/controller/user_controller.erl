-module(user_controller).
-export([handle_get/1, handle_post/1]).

-define(DEF_USERNAME, <<"请输入用户名">>).

%% 定义页面显示图片的个数
-define(DEF_PAGE_SIZE, 12).
-define(DEF_FPAGE_SIZE, 20).

%% 五种逻辑情况:
%% <1> 访问的当前用户不存在
%% <2> 用户登录, 访问自己的页面
%%     用户登录, 访问自己following的人的页面
%%     用户登录, 访问陌生人的页面
%% <3> 用户没登录
%%
%% UrlUsername是当前访问的用户.  /user/Username
%%
%% 传递给view的参数:
%% {login, Username, ext_myself, UrlUsername, UrlUserInfo, PageState} 
%% {login, Username, ext_following, UrlUsername, UrlUserInfo, PageState}
%% {login, Username, ext_no_following, UrlUsername, UrlUserInfo, PageState}
%% {logout_remember, ?DEF_USERNAME, undefined, UrlUsername, UrlUserInfo, PageState}
%% {logout_remember, Username, undefined, UrlUsername, UrlUserInfo, PageState}
%% {logout_no_remember, ?DEF_USERNAME, undefined, UrlUsername, UrlUserInfo, PageState}
handle_get(Req) ->
    {UrlUsername, UrlPageState} = parse_url(Req),
    case woomsg_user:get_user_all(UrlUsername) of
        [] ->
	    %% <1> 正在访问一个不存在的用户, 跳转到主页
            %% TODO: 单独的用户不存在页面
            Req:respond({302, [{"Location", "/"}], []});
	UrlUserInfo ->
            case woomsg_common:user_state(Req) of
                {login, Username} ->
	            %% <2> 用户登录
                    case Username =:= UrlUsername of
		        true ->
			%% 2.1 用户登录, 访问自己的页面
                            case UrlPageState of
				{fpage, _N} ->
				    %% TODO:
				    %% 修改为follow的逻辑, 目前是参看自己的照片
			            {PicCount, PicList} = woomsg_pic:get_pic_all(UrlUsername),
			            {fpage, PageIndex, PageStatePrev, PageStateNext, PageStateStart} = cal_page(UrlPageState, PicCount),
				    ResPicList = woomsg_pic_hook:process_pic_limit({PicCount, PicList}, Username, PageStateStart, ?DEF_PAGE_SIZE),
		                    Data = user_view:index(login, Username, ext_myself, UrlUsername, UrlUserInfo, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data});
			        _ ->
			            {PicCount, PicList} = woomsg_pic:get_pic_all(UrlUsername),
			            {page, PageIndex, PageStatePrev, PageStateNext, PageStateStart} = cal_page(UrlPageState, PicCount),
				    ResPicList = woomsg_pic_hook:process_pic_limit({PicCount, PicList}, Username, PageStateStart, ?DEF_PAGE_SIZE),
				    io:format("~p:~p:~p:~p~n", [PageStatePrev, PageStateNext, PageStateStart, ResPicList]),
		                    Data = user_view:index(login, Username, ext_myself, UrlUsername, UrlUserInfo, {page, PageIndex, PageStatePrev, PageStateNext, ResPicList}),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data})
			    end;
                        false ->
			    case woomsg_following:is_following(Username, UrlUsername) of
			        true ->
				    %% 2.2 用户登录, 访问自己following人的页面
		                    Data = user_view:index(login, Username, ext_following, UrlUsername, UrlUserInfo, undefined),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data});
				false ->
				    %% 2.3 用户登录, 访问陌生人人的页面
		                    Data = user_view:index(login, Username, ext_no_following, UrlUsername, UrlUserInfo, undefined),
                                    Req:respond({200, [{"Content-Type","text/html"}], Data})
			    end
                    end;
	        {logout_remember, undefined} ->
	            %% 用户没登录
		    Data = user_view:index(logout_remember, ?DEF_USERNAME, undefined, UrlUsername, UrlUserInfo, undefined),
                    Req:respond({200, [{"Content-Type","text/html"}], Data});
                {logout_remember, Username} ->
	            %% 用户没登录
		    Data = user_view:index(logout_remember, Username, undefined, UrlUsername, UrlUserInfo, undefined),
                    Req:respond({200, [{"Content-Type","text/html"}], Data});
		{logout_no_remember, undefined} ->
	            %% 用户没登录
		    Data = user_view:index(logout_no_remember, ?DEF_USERNAME, undefined, UrlUsername, UrlUserInfo, undefined),
                    Req:respond({200, [{"Content-Type","text/html"}], Data})
            end
    end.


handle_post(_Req) ->
    ok.

%% Internal APIs:
%%

%% 分页计算:
%% 返回结果:
%% {no_prev, no_next, Start}
%% {no_prev, next, Start}
%% {prev, no_next, Start}
%% {prev, next, Start}
cal_page(undefined, Count) ->
    cal_page({page, 1}, Count);
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
    end;
cal_page({fpage, Index}, Count) -> 
    Start = ?DEF_FPAGE_SIZE * (Index - 1) + 1,
    case Start + ?DEF_PAGE_SIZE =< Count of
	true ->
	    case Start =:=1 of
		true ->
		    {fpage, Index, no_prev, next, Start};
		false ->
		    {fpage, Index, prev, next, Start}
	    end;
	false ->
	    case Start =:= 1 of
		true ->
		    {fpage, Index, no_prev, no_next, Start};
		false ->
		    {fpage, Index, prev, no_next, Start}
	    end
    end.

%% URL的设计:
%% user/Username             -> 用户的首页
%% user/Username/page/N      -> 用户的照片(第Num页)
%% user/Username/fpage/N     -> 关注人的照片(第N页)
%%
%% 解析出URL中的用户名: Username
%% 返回:
%% {[], undefined}
%% {Username, undefined} 等价于 {Username, {page, 1}}
%% {Username, {page, N}}
%% {Username, {fpage, N}}
parse_url(Req) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "user" ->
	    {[], undefined};
	"user/" ->
	    {[], undefined};
        _ ->	
            %% <1> 去掉'user/'前缀
            %% erlang:length("user/") = 5
            %% user/Username/xxx -> Username/xxx
            PathSuffix = lists:sublist(Path, 6, erlang:length(Path) - 5),
            
            %% <2> 解析出URL中的Username
            %% Username/xxx     -> Username
            %% Username         -> Username
            %% Username?key=val -> Username?key=val (错误的用户名)
            %%
            %% 注意: 
            %% Usename?key=val这种形式的URL将返回错误的用户名
            %% 我们在设计中避免使用这样的URL
            Username = woomsg_util:list_index_prefix($/, PathSuffix),
            case Username =:= PathSuffix of
		true ->
		    {Username, undefined};
		false ->
		    %% <3> 去掉 user/Username/
		    %% user/Username/xxx -> xxx	
		    %%     fpage/N
		    %%     page/N	
		    PathSuffix1 = woomsg_util:list_index_suffix($/, PathSuffix),
		    case woomsg_util:list_index_prefix($/, PathSuffix1) of
			"page" ->
			    NPage = woomsg_util:list_index_suffix($/, PathSuffix1),
			    case woomsg_util:list_to_integer(NPage) of
				undefined ->
				    {Username, undefined};
				NPageVal ->
				    {Username, {page, NPageVal}}
			    end;
			"fpage" ->
			    FNPage = woomsg_util:list_index_suffix($/, PathSuffix1),
			    case woomsg_util:list_to_integer(FNPage) of
				undefined ->
				    {Username, undefined};
				FNPageVal ->
				    {Username, {fpage, FNPageVal}}
			    end;
			 _ ->
			     {Username, undefined}
                    end
            end
    end.
    
