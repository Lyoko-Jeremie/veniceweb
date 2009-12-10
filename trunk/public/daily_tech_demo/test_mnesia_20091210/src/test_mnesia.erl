-module(test_mnesia).
-export([init/0, create_table/1, cleanup/0,
         test_dirty_write/1, test_dirty_read/1,
	 test_transaction_write/1, test_transaction_read/1]).

-record(user, {id, name, age, description}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start().

cleanup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

%% Copy = ram | disc | disc_only
create_table(Copy) ->
    Copies = case Copy of
		 ram ->
		     {ram_copies, [node()]};
		 disc ->
		     {disc_copies, [node()]};
		 disc_only ->
		     {disc_only_copies, [node()]}
             end,
    mnesia:create_table(user, 
                        [Copies | [{type, set},
			           {attributes, record_info(fields, user)}]]).

test_dirty_write(Count)  ->
    erlang:statistics(wall_clock),
    erlang:statistics(runtime),
    handle_dirty_write(Count).
handle_dirty_write(Count) when Count > 0 ->
    mnesia:dirty_write({user, 
                        Count, 
                        "name - " ++ integer_to_list(Count),
		        -Count,
		        "this is a description - " ++ integer_to_list(Count)}),
    handle_dirty_write(Count - 1);
handle_dirty_write(_Count) ->  
    {_, Time1} = erlang:statistics(wall_clock),
    {_, Time2} = erlang:statistics(runtime),
    io:format("wall_clock time: ~p~n", [Time1]),
    io:format("runtime(CPU) time: ~p~n", [Time2]).

test_dirty_read(Key) ->
    erlang:statistics(wall_clock),
    erlang:statistics(runtime),
    Data = mnesia:dirty_read({user, Key}),
    {_, Time1} = erlang:statistics(wall_clock),
    {_, Time2} = erlang:statistics(runtime),
    io:format("Data: ~p~n", [Data]),
    io:format("wall_clock time: ~p~n", [Time1]),
    io:format("runtime(CPU) time: ~p~n", [Time2]).

test_transaction_write(Count) ->
    erlang:statistics(wall_clock),
    erlang:statistics(runtime),
    handle_transaction_write(Count).
handle_transaction_write(Count) when Count > 0 ->
    F = fun() ->
            mnesia:write({user, 
                          Count, 
                          "name - " ++ integer_to_list(Count),
		          -Count,
		          "this is a description - " ++ integer_to_list(Count)})
	end,
    mnesia:transaction(F),
    handle_transaction_write(Count - 1);
handle_transaction_write(_Count) ->  
    {_, Time1} = erlang:statistics(wall_clock),
    {_, Time2} = erlang:statistics(runtime),
    io:format("wall_clock time: ~p~n", [Time1]),
    io:format("runtime(CPU) time: ~p~n", [Time2]).

test_transaction_read(Key) ->
    erlang:statistics(wall_clock),
    erlang:statistics(runtime),
    F = fun() ->
		mnesia:read({user, Key})
	end,
    Data = mnesia:transaction(F),
    {_, Time1} = erlang:statistics(wall_clock),
    {_, Time2} = erlang:statistics(runtime),
    io:format("Data: ~p~n", [Data]),
    io:format("wall_clock time: ~p~n", [Time1]),
    io:format("runtime(CPU) time: ~p~n", [Time2]).
    
    


    
