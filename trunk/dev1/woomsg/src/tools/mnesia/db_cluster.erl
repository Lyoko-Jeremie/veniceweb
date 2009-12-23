-module(db_cluster).
-export([add_slave/1, delete_slave/1]).

%% 该函数在Mnesia cluster的Master节点上运行, 把SlaveNode添加到Mnesia cluster中.
add_slave(SlaveNode) ->
    %% start mnesia on slave node
    case rpc:call(SlaveNode, mnesia, start, []) of
	ok ->
	    case mnesia:change_config(extra_db_nodes, [SlaveNode]) of
		{ok, [SlaveNode]} ->
		     mnesia:change_table_copy_type(schema, SlaveNode, disc_copies);
		_ ->
		    {error, mnesia_change_config_error}
            end;
	_ ->
	    {error, mnesia_start_error_on_slave}
    end.

%% 该函数在Mnesia cluster的Master节点上运行, 把SlaveNode从Mnesia cluster中删除
%% 并且清空这个Slave node的所有数据.
%%
%% 删除Node的原理:
%% mnesia:del_table_copy(schema, Node)从mnesia cluster中删除Node节点, 如果mnesia
%% 正在该节点上运行, 则调用会失败.
%% 如果删除成功, 其它mnesia node不会再尝试连接到这个节点.
%% 注意: 
%% 如果Node上schema是disc_copies类型, 则需要清空整个目录, 否则mnesia再次在这个节点上
%% 启动的时候, 行为不确定.
%%
%% 注意:
%% 该函数成功的把一个SlaveNode从Mnesia cluster中删除, 但是在MasterNode上调用mnesia:info()
%% 查看的时候, 会发现SlaveNode在[stopped db nodes]中. 但实质上已经删除了(只是显示问题). 
%% mnesia:system_info(db_nodes)的结果已经更新了.
%%
delete_slave(SlaveNode) ->
    %% stop mnesia on slave node
    case rpc:call(SlaveNode, mnesia, stop, []) of
	stopped ->
	    case mnesia:del_table_copy(schema, SlaveNode) of
		{atomic, ok} ->
		    mnesia:delete_schema([SlaveNode]);
		_ ->
		    {error, mnesia_del_table_copy_error}
            end;
	_ ->
	    {error, mnesia_stop_error_on_slave}
    end.


