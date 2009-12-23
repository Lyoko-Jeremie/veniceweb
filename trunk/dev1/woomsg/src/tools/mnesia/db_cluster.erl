-module(db_cluster).
-export([add_slave/1, delete_slave/1]).

%% �ú�����Mnesia cluster��Master�ڵ�������, ��SlaveNode��ӵ�Mnesia cluster��.
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

%% �ú�����Mnesia cluster��Master�ڵ�������, ��SlaveNode��Mnesia cluster��ɾ��
%% ����������Slave node����������.
%%
%% ɾ��Node��ԭ��:
%% mnesia:del_table_copy(schema, Node)��mnesia cluster��ɾ��Node�ڵ�, ���mnesia
%% ���ڸýڵ�������, ����û�ʧ��.
%% ���ɾ���ɹ�, ����mnesia node�����ٳ������ӵ�����ڵ�.
%% ע��: 
%% ���Node��schema��disc_copies����, ����Ҫ�������Ŀ¼, ����mnesia�ٴ�������ڵ���
%% ������ʱ��, ��Ϊ��ȷ��.
%%
%% ע��:
%% �ú����ɹ��İ�һ��SlaveNode��Mnesia cluster��ɾ��, ������MasterNode�ϵ���mnesia:info()
%% �鿴��ʱ��, �ᷢ��SlaveNode��[stopped db nodes]��. ��ʵ�����Ѿ�ɾ����(ֻ����ʾ����). 
%% mnesia:system_info(db_nodes)�Ľ���Ѿ�������.
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


