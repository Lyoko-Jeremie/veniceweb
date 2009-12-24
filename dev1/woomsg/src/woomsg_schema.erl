-module(woomsg_schema).
-export([start/1, stop/1, install/3]).

-include("woomsg_schema.hrl").


start(DBCluster) ->
    case db_cluster:start_mnesia_nodes(DBCluster) of
	ok ->
	    case mnesia:wait_for_tables([user, 
                                         user_ext,
                                         following,
                                         pic,
                                         pic_comment,
                                         pic_tag, 
                                         session],
                                         100000) of
	        ok ->
	            ok;
 	        _ ->
	            {error, wait_for_tables_error}
            end;
	_ ->
	    {error, db_cluster_start_error}
    end.
    
stop(DBCluster) ->
    db_cluster:stop_mnesia_nodes(DBCluster).

%%
%% <1> 这个函数要求NodePool中的所有节点以及MasterNodes都已经启动, 彼此可以相互通讯
%% <2> 这个函数会安装干净的Mnesia db到所有的节点上, 并且使所有节点的
%%     mnesia:start/0
%% <3> 这个函数只需要在初始化系统的时候运行一次.
%%
%% @ ServerNodes 服务器节点列表, 只做通讯使用, 不存储人任何数据.
%% @ MasterNodes 主节点列表, 存储了所有数据的拷贝
%%               (MasterNodes只存储初始表, 表的所有fragment均匀分布在NodePool中)
%% @ NodePool   决定分片的存储节点(不包含MasterNodes)
install(ServerNodes, MasterNodes, NodePool)  ->
    %% start the cluster 
    %% TODO:
    %% error check
    DBCluster0 = [MasterNodes | NodePool ],
    DBCluster = lists:flatten([ServerNodes | DBCluster0]),
    db_cluster:stop_mnesia_nodes(DBCluster),
    mnesia:delete_schema(DBCluster),
    mnesia:create_schema(DBCluster),
    db_cluster:start_mnesia_nodes(DBCluster),

    %% user - set
    frag_db:create_table(user, 16, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, user)}]),
    %% user_ext - set
    frag_db:create_table(user_ext, 16, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, user_ext)}]),
    %% following - set
    frag_db:create_table(following, 16, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, following)}]),
    %% pic - set
    frag_db:create_table(pic, 64, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, pic)}]),
    %% pic_comment - bag
    frag_db:create_table(pic_comment, 64, NodePool, 
                         [{type, bag},
			  {disc_only_copies, MasterNodes}, 
			  {attributes, record_info(fields, pic_comment)}]),
    %% pic_tag - bag
    frag_db:create_table(pic_tag, 64, NodePool, 
                         [{type, bag},
			  {disc_only_copies, MasterNodes}, 
			  {attributes, record_info(fields, pic_tag)}]),

    %% session - set
    frag_db:create_table(session, 16, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, session)}]),
    ok.
    
    
   
    
    
