-module(msg_router).
-compile(export_all).

start() ->
    spawn(?MODULE, route_messages, []).

stop(RouterPid) ->
    RouterPid ! shutdown.

%% 注意:
%%  Addressee必须是Pid或者是注册的process-name, 否则出错!
send_chat_message(RouterPid, Addressee, MessageBody) ->
    RouterPid ! {send_chat_msg, Addressee, MessageBody}.

route_messages() ->
    receive
	{send_chat_msg, Addressee, MessageBody} ->
	    Addressee ! {recv_chat_msg, MessageBody},
	    route_messages();
	{recv_chat_msg, MessageBody} ->
	    io:format("Received: ~p~n", [MessageBody]),
	    route_messages();
	shutdown ->
	    io:format("Shutting down~n", []);
	Others ->
	    io:format("Warning! Received: ~p~n", [Others]),
	    route_messages()
    end.
    
