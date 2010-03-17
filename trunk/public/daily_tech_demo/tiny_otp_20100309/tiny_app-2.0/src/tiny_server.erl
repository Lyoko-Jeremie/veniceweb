%% save a key-value list by dict
-module(tiny_server).
-vsn("1.0").
-behaviour(gen_server).

-export([start_link/0]).
-export([add/2, delete/1, all/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).  %% 10秒钟

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc add the key value to server
add(Key, Value) ->
    gen_server:call(?SERVER, {add, Key, Value}).

%% @doc delete the key from the server
delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).

%% @doc get all key-value list
all() ->
    gen_server:call(?SERVER, {all}).


%% gen_server callbacks

init(_Args) ->
    {ok, dict:new(), ?TIMEOUT}. %% 注意:  这里的timeout会触发handle_info(timeout, State)

handle_call({add, Key, Value}, _From, State) ->
    Dict2 = dict:store(Key, Value, State),
    {reply, ok, Dict2};
handle_call({delete, Key}, _From, State) ->
    Dict2 = dict:erase(Key, State),
    {reply, ok, Dict2};
handle_call({all}, _From, State) ->
    Reply = dict:to_list(State),
    {reply, Reply, State};
handle_call(_Req, _From, State) ->
    {noreply, State}.
handle_cast(_Req, State) ->
    {noreply, State}.
handle_info(timeout, State) ->
    {ok, App} = application:get_application(),
    io:format("~p 2.0 running ....~n", [App]),
    {noreply, State, ?TIMEOUT};
handle_info(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



