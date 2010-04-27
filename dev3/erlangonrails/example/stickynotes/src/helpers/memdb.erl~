-module(blogdb).
-behaviour(gen_server).

%% 数据保存在内存中作为演示:

%% API
-export([start/0,
	 insert/2,
	 get/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Title,Body) ->
    gen_server:call(?MODULE,{insert,Title,Body}).

get() ->
    gen_server:call(?MODULE,get).

init([]) ->
    {ok, []}.

handle_call({insert,Title,Body}, _From, State) ->
    {Y,M,D} = date(),
    Posted = lists:flatten(io_lib:format("~p/~p/~p",[Y,M,D])),
    Record = [{title,Title},{body,Body},{posted,Posted}],
    State1 = [Record|State],
    {reply, ok, State1};

handle_call(get,_From,State) ->
    Reply = lists:sort(fun(A,B) -> 
			       proplists:get_value(posted,A) >= proplists:get_value(posted,B) 
		       end,State),
    {reply,Reply,State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
