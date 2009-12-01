-module(http_server).
-compile(export_all).

-define(SERVER, ?MODULE).

start() ->
    inets:start(),
    register(?SERVER, spawn(?MODULE, recv, ["", undefined])).

get_url(Url) ->
    ?SERVER ! {get_url, Url, self()},
    receive
	{?SERVER, Data} ->
	    Data
    end.

query_baidu(Data) ->
    %% URL: 
    %% "http://www.baidu.com/s?wd=email",
    %% "http://www.baidu.com/s?wd=email+liqiang",
    BaiduQuery = "http://www.baidu.com/s?wd=email+" ++ Data,
    get_url(BaiduQuery).
    

stop() ->
    ?SERVER ! {stop, normal},
    inets:stop().
    

recv(State, Pid) ->
    %% 
    receive
	{http, {_Ref, stream_start, _Headers}} ->
	    io:format("stream_start~n"),
	    recv(State, Pid);
        {http, {_Ref, stream, Result}} ->
	    Result1 = util:remove_html_tag(Result),
	    io:format("stream~n"),
	    case util:parse_email(Result1) of
		[] ->
		    recv(State, Pid);
		Any ->
		    recv([Any | State], Pid)
            end;
        {http, {_Ref, stream_end, _Headers}} ->
	    io:format("stream_end~n"),
	    case Pid of
		undefined ->
		    "";
		_ ->
		    Pid ! {?SERVER, State}
            end,
	    recv("", undefined);
        {http, {_Ref, {error, Why}}} ->
	    io:format("error! Closed due to: ~w~n", [Why]),
	    recv("", undefined);
        {get_url, Url, NewPid} ->
	    http:request(get,
			 {Url, []},
			 [],
			 [{sync, false}, {stream, self}, {version, 1.1}]),
	    recv("", NewPid); %% need to sync
	{stop, _Reason} ->
	    io:format("http_server stopping...~n"),
	    http_server_stopped
    end.
