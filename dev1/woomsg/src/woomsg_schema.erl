-module(woomsg_schema).
-export([install/2]).

-include("woomsg_schema.hrl").

%% @ MasterNodes ���ڵ�, �洢���������ݵĿ���
%%              (MasterNodes������NodePool�е�һ����)
%% @ NodePool ������Ƭ�Ĵ洢�ڵ�
install(MasterNodes, NodePool)  ->
    %% start the cluster 
    db_cluster:stop_mnesia_nodes(NodePool),
    mnesia:delete_schema(NodePool),
    mnesia:create_schema(NodePool),
    db_cluster:start_mnesia_nodes(NodePool),

    %% user
    frag_db:create_table(user, 16, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, user)}]),
    %% user_ext
    frag_db:create_table(user_ext, 16, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, user_ext)}]),
    %% following
    frag_db:create_table(following, 16, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, following)}]),
    %% pic
    frag_db:create_table(pic, 64, NodePool, 
                         [{type, set},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, pic)}]),
    %% pic_comment
    frag_db:create_table(pic_comment, 64, NodePool, 
                         [{type, bag},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, pic_comment)}]),
    %% pic_tag
    frag_db:create_table(pic_tag, 64, NodePool, 
                         [{type, bag},
			  {disc_copies, MasterNodes}, 
			  {attributes, record_info(fields, pic_tag)}]),
    ok.
    
    
   
    
    
