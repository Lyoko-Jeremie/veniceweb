-module(message_store).
-compile([export_all]).

%%
%% 这个模块使用Mnesia存储用户的消息
%%

%% Windows Path:
-include_lib("C:\\Program Files\\erl5.7.3\\lib\\stdlib-1.16.3\\include\\qlc.hrl").

-define(SERVER, ?MODULE).

-record(chat_message, {addressee, body, create_on}).

save_message(Addressee, Body) ->
    global:send(?SERVER, {save_msg, Addressee, Body}).

find_message(Addressee) ->
    global:send(?SERVER, {find_msgs, Addressee, self()}),
    receive
	{ok, Messages} ->
	    Messages
    end.

start() ->
    server_util:start(?SERVER, {?MODULE, run, [true]}).

stop() ->
    server_util:stop(?SERVER).

run(FirstTime) ->
    if
	FirstTime == true ->
	    init_store(),
	    run(false);
	true ->
	    receive
		{save_msg, Addressee, Body} ->
		    store_message(Addressee, Body),
		    run(FirstTime);
		{find_msgs, Addressee, Pid} ->
		    Message = get_messages(Addressee),
		    Pid ! {ok, Message},
		    run(FirstTime);
		shutdown ->
		    mnesia:stop(),
		    io:format("Shutting down...~n", [])
            end
    end.

%% 删除所有的消息
delete_messages(Messages) ->
    F = fun() ->
		lists:foreach(fun(Msg) ->
				      mnesia:delete_object(Msg)
			      end, Messages)
	end,
    mnesia:transaction(F).

%% 获取Addressee对应的所有消息
get_messages(Addressee) ->
    F = fun() ->
		Query = qlc:q([M || M <- mnesia:table(chat_message),
				    M#chat_message.addressee =:= Addressee]),
		Results = qlc:e(Query),
		delete_messages(Results),
		lists:map(fun(Msg) ->
				  Msg#chat_message.body
			  end, Results)
	end,
    {atomic, Messages} = mnesia:transaction(F),
    Messages.

%% 存储一条消息
store_message(Addressee, Body) ->
    F = fun() ->
		{_, CreateOn, _} = erlang:now(),
		mnesia:write(#chat_message{addressee=Addressee,
					   body = Body,
					   create_on = CreateOn})
	end,
    mnesia:transaction(F).

%% 初始化Mnesia, 创建表格
init_store() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    try	mnesia:table_info(chat_message, type) of
        _ ->
	    ok
    catch
	exit: _ ->
	    mnesia:create_table(chat_message, [{attributes, record_info(fields, chat_message)},
					       {type, bag},
					       {disc_copies, [node()]}])
    end.
