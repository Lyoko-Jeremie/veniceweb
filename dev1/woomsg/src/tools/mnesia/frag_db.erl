-module(frag_db).
-export([create_table/3, create_table/4,
	 activate/1, activate/2, deactivate/1,
	 add_frag/1, add_frag/2, add_num_frag/2,
	 del_frag/1, del_num_frag/2,
	 get_dist/1, get_frag_size/1, get_frag_memory/1, get_size/1, get_memory/1,
	 get_num_fragments/1, get_node_pool/1]).

%% 创建一个具有frag属性的Table
%% @NFragment integer(), 需要创建的Fragment的数量
%% @NodePool 节点池
%%           默认使用当前节点[node()]作为节点池
%%           我们也可以使用mnesia:system_info(db_nodes), 把mnesia cluster中
%%           所有的node组成节点池
%% @TabDef   正常的表格定义
%%
%% Bug# 
%% 使用下面的API创建的分片表格, 即使在TabDef中定义了{disc_copies, Nodes},
%% 创建的原始表和所有的分片表都是ram_copies类型的.
%%
%% 所以我们采用了另外的方式创建，先创建一个原始表, 然后不断的增加frag. 增加的frag类型和
%% 原始表格一样.
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
    

%% 激活一个Table的分片属性
%%
%% @NodePool 节点池
%%          
activate(Table) ->
    mnesia:change_table_frag(Table, {activate, [{node_pool, [node()]}]}).
activate(Table, NodePool) ->
    mnesia:change_table_frag(Table, {activate, [{node_pool, NodePool}]}).

%% 解除表的分片属性, 分片数必须是1, 并且其它表没有引用这个表
deactivate(Table) ->
    mnesia:change_table_frag(Table, deactivate).

%% 增加一个frag
%% 
%% 注意:
%% 默认表格式什么类型, 新增的frag就是什么类型, 如果默认表格式disc_copies, 则新增的frag也是该类型.
add_frag(Table) ->
    mnesia:change_table_frag(Table, {add_frag, get_dist(Table)}).
add_frag(Table, NodesOrDist) ->
    mnesia:change_table_frag(Table, {add_frag, NodesOrDist}).

%% 增加多个frag
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

%% 删除一个frag
del_frag(Table) ->
    mnesia:change_table_frag(Table, del_frag).

%% 删除多个frag
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


%% 属性操作
get_dist(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    Dist = mnesia:activity(transaction, InfoFun, [frag_dist], mnesia_frag),
    Dist.

%% 返回{Name, Size}列表, Name是片段的名字, Size是片段的记录条数
get_frag_size(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    FragSize = mnesia:activity(transaction, InfoFun, [frag_size], mnesia_frag),
    FragSize.

%% 返回{Name, Memory}列表, Name是片段的名字, Memory是片段所占用的内存
get_frag_memory(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    FragMem = mnesia:activity(transaction, InfoFun, [frag_memory], mnesia_frag),
    FragMem.

%% 返回所有片段的总尺寸
get_size(Table) ->
    InfoFun = fun(Item) -> mnesia:table_info(Table, Item) end,
    Size = mnesia:activity(transaction, InfoFun, [size], mnesia_frag),
    Size.

%% 返回所有片段占用的总内存
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

