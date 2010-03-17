-module(tiny).
-behaviour(application).
-export([start/2, stop/1, loop/0]).

%% 返回 {ok, pid()}
start(_Type, _Args) ->
    {ok, proc_lib:spawn_link(?MODULE, loop, [])}.

stop(_State) ->
    ok.

loop() ->
    receive Msg ->
        io:format("receive message:~p~n", [Msg]),
	loop()
    after 10000 -> %% 单位是毫秒(1/1000秒)
        {ok, Application} = application:get_application(self()), 
	    io:format("~p v1.0 running ...  ~n", [Application]),
	    loop()
    end,
    loop().
    

