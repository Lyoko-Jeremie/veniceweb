-module(login_controller,[Env]).
-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"login/index.html",[]};

%% 处理POST请求
handle_request("create",[]) ->
    Usr = erails_var:get_param("usr", Env),
    Pwd = erails_var:get_param("pwd", Env),
    %% 登录验证:
    case Usr =:= "liqiang" andalso Pwd =:= "123456" of
	true ->
	    %% 登录成功后更新session信息.
	    erails_var:set_session_data("usr_id", "liqiang", Env),
	    error_logger:info_msg("liqiang login success! ~n"),
	    {redirect, "/home/new"};
	false ->
	    error_logger:info_msg("liqiang login failed! ~n"),
	    {redirect, "/login/index"}
    end.
    

before_filter() ->
    FilterOnly = ["index", "create"],
    case lists:member(erails_var:get_action(Env), FilterOnly) of
	true ->
	    ok;
	false ->
	    {text, "no action!!!"}
    end.
