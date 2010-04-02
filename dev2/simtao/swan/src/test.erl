-module(test).
-export([init_mysql/0,
         get_buildin_keyword/0,
         insert_buildin_keyword/0]).

-define(DB_HOSTNAME, "localhost").
-define(DB_USERNAME, "root").
-define(DB_PORT,     3306).
-define(DB_PASSWORD, "user@123").
-define(DB_DATABASE, "simtao_db").
-define(DB_POOL_PID, simtaodb).
-define(DB_POOL_SIZE,50).

%%
%% 功能:
%%  初始化MySQL链接, 初始化一个连接池, pool里面一个DB_POOL_SIZE个链接.
%%
init_mysql() ->
    erlangrail_mysql:start([{pool_id,  ?DB_POOL_PID},
                            {hostname, ?DB_HOSTNAME},
                            {port,     ?DB_PORT},
		            {username, ?DB_USERNAME}, 
                            {password, ?DB_PASSWORD},
		            {database, ?DB_DATABASE},
                            {poolsize, ?DB_POOL_SIZE}]).

get_buildin_keyword() ->
    SqlStatement = "select keyword_text from buildin_keyword",
    {data, SqlRes} = mysql:fetch(?DB_POOL_PID, SqlStatement),
    case SqlRes of
        {mysql_result,_FieldInfo, [], _AffectRows, _Error} ->
            [];
	_ ->
	    KeywordRet = lists:foldl(fun([Item], AccIn) ->
					 io:format("~ts~n", [Item]),
                                         [ Item | AccIn]
                                     end, [], mysql:get_result_rows(SqlRes)),
	    lists:reverse(KeywordRet)
    end.

%% INSERT INTO usr(username, email, password) VALUES (Username, Email, Password)
insert_buildin_keyword() ->
    SqlStatement = "INSERT INTO `buildin_keyword` (`buildin_keyword_id`, `buildin_keyword_group_id`, `keyword_text`, `symbol`) VALUES (1001, 5, '行货手机123', NULL)",
    mysql:fetch(?DB_POOL_PID, SqlStatement).


    
