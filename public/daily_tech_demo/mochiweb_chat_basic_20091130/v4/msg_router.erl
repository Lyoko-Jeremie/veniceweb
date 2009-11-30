-module(msg_router).
-compile(export_all).

-define(SERVER, ?MODULE).

start() ->
    server_util:start(?SERVER, {?MODULE, route_messages, [dict:new()]}),
    message_store:start().

stop() ->
    server_util:stop(?SERVER),
    message_store:stop().

send_chat_message(Addressee, MessageBody) ->
    global:send(?SERVER, {send_chat_msg, Addressee, MessageBody}).

%% 注册: ClientName & ClientPid, 每个Client一个process处理它收到的消息.
register_nick(ClientName, ClientPid) ->
    global:send(?SERVER, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
    global:send(?SERVER, {unregister_nick, ClientName}).

route_messages(Clients) ->
    receive
	{send_chat_msg, ClientName, MessageBody} ->
	    case dict:find(ClientName, Clients) of
		{ok, ClientPid} ->
		    ClientPid ! {printmsg, MessageBody};
		error ->
		    %% 暂时保存收到的这条消息
		    message_store:save_message(ClientName, MessageBody),
		    io:format("Archived message for ~p~n", [ClientName])
            end,
	    route_messages(Clients);
	{register_nick, ClientName, ClientPid} ->
	    %% 新用户注册，产看是否有消息
	    Messages = message_store:find_message(ClientName),
	    lists:foreach(fun(Msg) -> ClientPid ! {printmsg, Msg} end, Messages),
	    route_messages(dict:store(ClientName, ClientPid, Clients));
	{unregister_nick, ClientName} ->
	    case dict:find(ClientName, Clients) of
		{ok, ClientPid} ->
		    ClientPid ! stop,
		    route_messages(dict:erase(ClientName, Clients));
		error ->
		    io:format("Error! Unknown client: ~p~n", [ClientName]),
		    route_messages(Clients)
            end;
	shutdown ->
	    io:format("Shutting down~n", []);
	Others ->
	    io:format("Warning! Received: ~p~n", [Others]),
	    route_messages(Clients)
    end.
    
