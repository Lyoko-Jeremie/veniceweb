-module(msg_router).
-compile(export_all).

-define(SERVER, ?MODULE).

start() ->
    Pid = spawn(?MODULE, route_messages, [dict:new()]),
    erlang:register(?SERVER, Pid).

stop() ->
    ?SERVER ! shutdown.

send_chat_message(Addressee, MessageBody) ->
    ?SERVER ! {send_chat_msg, Addressee, MessageBody}.

register_nick(ClientName, PrintFun) ->
    ?SERVER ! {register_nick, ClientName, PrintFun}.

unregister_nick(ClientName) ->
    ?SERVER ! {unregister_nick, ClientName}.

route_messages(Clients) ->
    receive
	{send_chat_msg, ClientName, MessageBody} ->
	    ?SERVER ! {recv_chat_msg, ClientName, MessageBody},
	    route_messages(Clients);
	{recv_chat_msg, ClientName, MessageBody} ->
	    %% 消息的分发
	    case dict:find(ClientName, Clients) of
		{ok, PrintFun} ->
		    PrintFun(MessageBody);  %% 使用动态绑定的消息print函数
		error ->
		    io:format("Unknown client~n", [])
            end,
	    route_messages(Clients);
	{register_nick, ClientName, PrintFun} ->
	    route_messages(dict:store(ClientName, PrintFun, Clients));
	{unregister_nick, ClientName} ->
	    route_messages(dict:erase(ClientName, Clients));
	shutdown ->
	    io:format("Shutting down~n", []);
	Others ->
	    io:format("Warning! Received: ~p~n", [Others]),
	    route_messages(Clients)
    end.
    
