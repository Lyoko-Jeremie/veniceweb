-module(chat_client).
-compile(export_all).

send_message(RouterPid, Addressee, MessageBody) ->
    msg_router:send_chat_message(RouterPid, Addressee, MessageBody).
