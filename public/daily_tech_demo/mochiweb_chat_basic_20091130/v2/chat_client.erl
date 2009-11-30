-module(chat_client).
-compile(export_all).

register_nickname(Nickname) ->
    msg_router:register_nick(Nickname, fun(Msg) ->
					       chat_client:print_msg(Nickname, Msg)
				       end).

unregister_nickname(Nickname) ->
    msg_router:unregister_nick(Nickname).

send_message(Addressee, MessageBody) ->
    msg_router:send_chat_message(Addressee, MessageBody).

print_msg(Who, MessageBody) ->
    io:format("~p received: ~p~n", [Who, MessageBody]).

start_router() ->
    msg_router:start().
