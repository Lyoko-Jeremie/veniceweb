-module(mydriver).
-export([test/1, start/0, stop/0]).
-export([twice/1, sum/2]).

-define(PROC_NAME, myport_driver_proc).

test(Count)->
    %% wall_clock: 代码段的执行时间
    %% runtime:    CPU时间(注意多核CPU的情况)
    statistics(wall_clock), 
    statistics(runtime),
    lists:foreach(fun(X) -> twice((X rem 128)) end, lists:seq(1, Count)),
    {_,Time1}=statistics(wall_clock),
    {_,Time2}=statistics(runtime),
    io:format("Wall clock:~p ms, run-time:~p ms~n",[Time1,Time2]).

%% 依赖:
%% 在Linux平台上，我们需要把动态链接库mydriver.so拷贝到
%% mydriver.beam相同的目录下,才能正常启动.
start()->
    start("mydriver").

start(ShareLib) ->
    case erl_ddll:load_driver(".",ShareLib) of
	ok ->
	    ok;
	{error, already_loaded} ->
	    ok;
	 _->
	    exit({error, could_not_load_driver})
    end,
    spawn(fun() ->
	          init(ShareLib)
	  end).

%% 创建端口连接进程，并进入消息循环.
init(ShareLib) ->
    register(?PROC_NAME, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ShareLib},[]),
    io:format("port_driver started: ~p~n", [Port]),
    loop(Port).

stop() ->
    ?PROC_NAME ! stop.

twice(X) ->
    call_port({twice, X}).
sum(X, Y) ->
    call_port({sum, X, Y}).

%% 向端口连接进程发送消息，并等候响应
call_port(Msg) ->
    ?PROC_NAME ! {call, self(), Msg},
    receive
	{?PROC_NAME, Result} ->
	    Result
    end.

%% 一个概念: 端口连接进程
%%  创建端口的进程称为端口连接进程，简单说也就是调用了open_port函数来创建了port进程.
%%
%% 向Port发送数据(其中PidC是端口连接进程的Pid,):
%%  Port ! {PidC, {command, Data}} 
%% 接收来自外部的数据:
%%  receive
%%      {Port, {data, Data}} ->
%%          ....handle the external Data
%%  end
loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
	    receive 
		{Port, {data, Data}} ->
		    Caller ! {?PROC_NAME, decode(Data)}
            end,
	    loop(Port);
        stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    io:format("port_driver stopped: ~p~n", [Port]),
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("port_driver_terminated: ~p~n",[Reason]),
	    exit(port_driver_terminated)
    end.

%% 根据通讯协议进行编码
encode({twice, X}) ->
    [1, X];
encode({sum, X, Y}) ->
    [2, X, Y].
decode([Int]) ->
    Int.
    
