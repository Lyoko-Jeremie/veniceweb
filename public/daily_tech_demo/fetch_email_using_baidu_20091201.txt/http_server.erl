-module(http_server).
-compile(export_all).

-define(SERVER, ?MODULE).

start() ->
    inets:start(),
    register(?SERVER, spawn(?MODULE, recv, [])).

get_url(Url) ->
    ?SERVER ! {get_url, Url, self()}.

query_baidu() ->
    %%BaiduQuery = "http://www.baidu.com/s?wd=email",
    BaiduQuery1 = "http://www.baidu.com/s?bs=email&wd=email+liqiang",
    get_url(BaiduQuery1).
    

stop() ->
    ?SERVER ! {stop, normal},
    inets:stop().
    

recv() ->
    %% 接受HTTP应答消息和发送HTTP请求消息
    receive
	{http, {_Ref, stream_start, _Headers}} ->
	    recv();
        {http, {_Ref, stream, Result}} ->
	    Result1 = util:remove_html_tag(Result),
	    %%io:format("~p~n", [Result1]),
	    {ok, IoDevice} = file:open("baidu.txt", [append]),
	    file:write(IoDevice, Result1),
	    io:format("writing data to baidu.txt~n"),
            util:process_html(Result1),
	    recv();
        {http, {_Ref, stream_end, _Headers}} ->
	    recv();
        {http, {_Ref, {error, Why}}} ->
	    io:format("error! Closed due to: ~w~n", [Why]),
	    recv();
        {get_url, Url, _Pid} ->
	    http:request(get,
			 {Url, []},
			 [],
			 [{sync, false}, {stream, self}, {version, 1.1}]),
	    recv();
	{stop, _Reason} ->
	    io:format("http_server stopping...~n"),
	    http_server_stopped
    end.
