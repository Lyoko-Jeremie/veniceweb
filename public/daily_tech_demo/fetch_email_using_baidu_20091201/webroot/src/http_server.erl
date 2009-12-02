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

%% URL: 
%% <1> ASCII (Data=liqiang)
%% http://www.baidu.com/s?wd=email
%% http://www.baidu.com/s?wd=email+liqiang
%% http://www.baidu.com/s?wd=email+liqiang+keywork
%%
%% <2> Unicode (Data=李强)
%% http://www.baidu.com/s?wd=email
%% http://www.baidu.com/s?wd=email+%C0%EE%C7%BF
%% http://www.baidu.com/s?wd=email+%C0%EE%C7%BF+keyword
%%
%% Note:
%%  Baidu using GB2312 encoding, not UTF8
query_baidu(Data) ->
    BaiduPrefix = "http://www.baidu.com/s?wd=",
    UnicodeList = unicode:characters_to_binary(Data),
    BaiduQuery = BaiduPrefix ++ "email+" ++ "%C0%EE%C7%BF",%% binary_to_list(UnicodeList),
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
		    NewState = lists:foldl(fun(X, Acc) -> [X | Acc] end, State, Any),
		    recv(NewState, Pid)
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
