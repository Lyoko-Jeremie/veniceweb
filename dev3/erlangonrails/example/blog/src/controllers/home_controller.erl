-module(home_controller,[Env]).
-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    Blogs = blogdb:get(),
    {render,"home/index.html",[{blogs, Blogs}]};

handle_request("new", []) ->
    {render, "home/new.html", []};

%% 处理POST的数据
handle_request("create", []) ->
    Title = erails_var:get_param("title", Env),
    Body = erails_var:get_param("body", Env),
    %% Validation - 需要一个标题
    if
	Title =:= undefined orelse length(Title) =:= 0 ->
	    erails_var:flash({notice, "请输入文章标题!"}, Env),
	    {redirect, "/home/new"};
	true ->
	    erails_var:flash({notice, "文章创建成功!"}, Env),
	    blogdb:insert(Title, Body),
	    {redirect, "/home"}
    end.


before_filter() ->
    FilterOnly = ["index", "new", "create"],
    case lists:member(erails_var:get_action(Env), FilterOnly) of
	true ->
	    case erails_var:get_session_data("usr_id", Env) of
		undefined ->
		    %% 用户没有登录:
		    {redirect, "/login"};
		_A ->
		    ok
            end;
	false ->
	    {text, "no action!!!"}
    end.
