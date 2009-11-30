-module(chat_client).
-compile(export_all).

register_nickname(Nickname) ->
    Pid = spawn(?MODULE, handle_messages, [Nickname]),
    msg_router:register_nick(Nickname, Pid).

unregister_nickname(Nickname) ->
    msg_router:unregister_nick(Nickname).

send_message(Addressee, MessageBody) ->
    msg_router:send_chat_message(Addressee, MessageBody).

handle_messages(Nickname) ->
    receive
	{printmsg, MessageBody} ->
	    io:format("~p received: ~p~n", [Nickname, MessageBody]),
	    handle_messages(Nickname);
	stop ->
	    ok
    end.

start_router() ->
    msg_router:start().
