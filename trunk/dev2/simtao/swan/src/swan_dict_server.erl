-module(swan_dict_server).
-behaviour(gen_server).

-define(UNICODE_TABLE, unicodetable).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0, get_tableid/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_tableid() ->
    gen_server:call(?MODULE, {get, tableid}).

init([]) ->
    %% init the unicode table
    TableId = swan_dict:new_table_unicode(?UNICODE_TABLE),
    {ok, TableId}.

handle_call({get, tableid}, _From, TableId) ->
    Reply = TableId,
    {reply, Reply, TableId};
handle_call(stop, _From, TableId) ->
    {stop, normal, stopped, TableId}.

handle_cast(_Msg, TableId) ->
    {noreply, TableId}.

handle_info(_Info, TableId) ->
    {noreply, TableId}.

terminate(_Reason, TableId) ->
    swan_dict:delete_table_unicode(TableId),
    ok.

code_change(_OldVsn, TableId, _Extra) ->
    {ok, TableId}.
    
