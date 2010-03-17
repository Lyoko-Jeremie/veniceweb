%% 通过application来启动监控树.
%%
-module(tiny).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).  %% application callback
-export([init/1]).           %% supervisor callback

-define(SERVER, tiny_sup).

%% application callback
start(_Type, _Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []). %% 监控树启动, 通过?MODULE:init/1内置的策略

stop(_State) ->
    ok.

%% supervisor callback
%% 
%% return:
%% {ok, Strategy, [Worker1, Worker2, ...]}
%% Strategy = {RestartStrategy, MaxRestarts, Time}
%% Worker   = {Tag, {Mod, Func, ArgList}, Tag标记这个"工作进程"
%%             Restart,           permanent | transient | temorary
%%                                永久(重启)|瞬间(正常退出才重启)|临时(不重启)
%%             Shutdown,          终止时间
%%             Type,              worker | supervisor
%%             [Mod]}             如果子进程是一个监控进程或者gen_server的行为模式的回调函数,
%%                                制定了回调模块的名字.
init(_Args) ->
    SupStra = {one_for_all, 2, 1}, %% {RestartStrategy, MaxRestarts, Time}
                                   %% 后两项决定了: 重启频率
    TinyServer = {tiny_server, {tiny_server, start_link, []},
                  permanent, 10, worker, [tiny_server]},
    {ok, {SupStra,[TinyServer]}}.
    

