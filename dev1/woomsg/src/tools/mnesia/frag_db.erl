-module(frag_db).
-export([create_table/3, create_table/4,
	 activate/1, activate/2, deactivate/1,
	 add_frag/1, add_frag/2, add_num_frag/2,
	 del_frag/1, del_num_frag/2,
	 get_dist/1, get_frag_size/1, get_frag_memory/1, get_size/1, get_memory/1,
	 get_num_fragments/1, get_node_pool/1]).

%% ����һ������frag���Ե�Table
%% @NFragment integer(), ��Ҫ������Fragment������
%% @NodePool �ڵ��
%%           Ĭ��ʹ�õ�ǰ�ڵ�[node()]��Ϊ�ڵ��
%%           ����Ҳ����ʹ��mnesia:system_info(db_nodes), ��mnesia cluster��
%%           ���е�node��ɽڵ��
%% @TabDef   �����ı����
%%
%% Bug# 
%% ʹ�������API�����ķ�Ƭ���, ��ʹ��TabDef�ж�����{disc_copies, Nodes},
%% ������ԭʼ������еķ�Ƭ����ram_copies���͵�.
%%
%% �������ǲ���������ķ�ʽ�������ȴ���һ��ԭʼ��, Ȼ�󲻶ϵ�����frag. ���ӵ�frag���ͺ�
%% ԭʼ���һ��.
%%
%% create_table(Table, NFragment, TabDef) ->
%%     FragProps = [{n_fragments, NFragment}, {node_pool, [node()]}],
%%     mnesia:create_table(Table, [{frag_properties, FragProps} | TabDef]).
%% create_table(Table, NFragment, NodePool, TabDef) ->
%%     FragProps = [{n_fragments, NFragment}, {node_pool, NodePool}],
%%     mnesia:create_table(Table, [{frag_properties, FragProps} | TabDef]).
create_table(Table, NFragment, TabDef) ->
    mnesia:create_table(Table, TabDef),
    activate(Table),
    add_num_frag(Table, NFragment).
create_table(Table, NFragment, NodePool, TabDef) ->
    mnesia:create_table(Table, TabDef),
    activate(Table, NodePool),
    add_num_frag(Table, NFragment).
    

%% ����һ��Table�ķ�Ƭ����
%%
%% @NodePool �ڵ��
%%          
activate(Table) ->
    mnesia:change_table_frag(Table, {activate, [{node_pool, [node()]}]}).
activate(Table, NodePool) ->
    mnesia:change_table_frag(Table, {activate, [{node_pool, NodePool}]}).

%% �����ķ�Ƭ����, ��Ƭ��������1, ����������û�����������
deactivate(Table) ->
    mnesia:change_table_frag(Table, deactivate).

%% ����һ��frag
%% 
%% ע��:
%% Ĭ�ϱ��ʽʲô����, ������frag����ʲô����, ���Ĭ�ϱ��ʽdisc_copies, ��������fragҲ�Ǹ�����.
add_frag(Table) ->
    mnesia:change_table_frag(Table, {add_frag, get_dist(Table)}).
add_frag(Table, NodesOrDist) ->
    mnesia:change_table_frag(Table, {add_frag, NodesOrDist}).

%% ���Ӷ��frag
add_num_frag(Table, Num) when is_integer(Num) ->
    case Num > 0 of
	true ->
	    handle_add_num_frag(Table, Num);
	false ->
	    {error, num_not_positive}
    end;
add_num_frag(_Table, _Num) ->
    {error, num_not_integer}.

handle_add_num_frag(Table, Num) when Num > 0 ->
    add_frag(Table),
    handle_add_num_frag(Table, Num - 1);
handle_add_num_frag(_Table, _Num) ->
    ok.

%% ɾ��һ��frag
del_frag(Table) ->
    mnesia:change_table_frag(Table, del_frag).

%% ɾ�����frag
del_num_frag(Table, Num) when is_integer(Num) ->
    case Num > 0 of
	true ->
	    case get_num_fragments(Table) > Num of
		true ->
		    handle_del_num_frag(Table, Num);
		false ->
		    {error, frag_balance_error}
            end;
	false ->
	    {error, num_not_positive}
    end;
del_num_frag(_Table, _Num) ->
    {error, num_not_integer}.

handle_del_num_frag(Table, Num) when Num > 0 ->
    del_frag(Table),
    handle_del_num_frag(Table, Num - 1);
handle_del_num_frag(_Table, _Num) ->
    ok.


%% ���Բ���
get_dist(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    Dist = mnesia:activity(transaction, InfoFun, [frag_dist], mnesia_frag),
    Dist.

%% ����{Name, Size}�б�, Name��Ƭ�ε�����, Size��Ƭ�εļ�¼����
get_frag_size(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    FragSize = mnesia:activity(transaction, InfoFun, [frag_size], mnesia_frag),
    FragSize.

%% ����{Name, Memory}�б�, Name��Ƭ�ε�����, Memory��Ƭ����ռ�õ��ڴ�
get_frag_memory(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    FragMem = mnesia:activity(transaction, InfoFun, [frag_memory], mnesia_frag),
    FragMem.

%% ��������Ƭ�ε��ܳߴ�
get_size(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    Size = mnesia:activity(transaction, InfoFun, [size], mnesia_frag),
    Size.

%% ��������Ƭ��ռ�õ����ڴ�
get_memory(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    Mem = mnesia:activity(transaction, InfoFun, [memory], mnesia_frag),
    Mem.

get_num_fragments(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    NFragments = mnesia:activity(transaction, InfoFun, [n_fragments], mnesia_frag),
    NFragments.

get_node_pool(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    NodePool = mnesia:activity(transaction, InfoFun, [node_pool], mnesia_frag),
    NodePool.

