-module(erails_session_server).
-behaviour(gen_server).

-export([start/0,
	 new_session/1, delete_session/1,
	 get_session_data/1,set_session_data/3,remove_session_data/2]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-record(erails_session, {sid,data,ttl}).
-define(SID_LEN, 16).

%% Erlang On Rails的session管理模块:
%%
%% 内部维护一个ETS表, 数据格式是{erails_session, sid, data, ttl},
%% sid: 是key, 是随机产生的GUID
%% data: 是一个[{Key, Val}]的proplists.
%% ttl: 当前版本的ttl默认都是0.



%% API
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE,[set,named_table,{keypos, 2}]),
    {ok, undefined}.

%% @spec new_session(Data) -> Sid:string()
%%
%% Data就是Sid: 如果Data是undefined或者在ETS表中不存在, 则产生一个新的SID.
%%             如果Data在ETS表中已经存在, 则返回.
new_session(Data) ->
    gen_server:call(?MODULE,{new_session,Data}).

%% @spec get_session_data(Sid) -> [{Key, val}] | []
get_session_data(Sid) ->
    gen_server:call(?MODULE,{get_session_data,Sid}).

set_session_data(Sid,Key,Value) ->
    gen_server:call(?MODULE,{set_session_data,Sid,Key,Value}).

delete_session(Sid) ->
    gen_server:call(?MODULE,{delete_session,Sid}).

remove_session_data(Sid,Key) ->
    gen_server:call(?MODULE,{remove_session_data,Sid,Key}).


%%% Callbacks
handle_call({new_session,Cookie}, _From, _State) ->
    NewId = case Cookie of
		undefined ->
		    Sid = erails_guid:get_with_datetime(?SID_LEN),
                    Session = #erails_session{sid=Sid,data=[],ttl=0},
                    ets:insert(?MODULE,Session),
                    Sid;
		Any ->
		    case ets:member(?MODULE, Any) of
			true ->
			    Any;
			false -> 
			    Sid = erails_guid:get_with_datetime(?SID_LEN),
                            Session = #erails_session{sid=Sid,data=[],ttl=0},
                            ets:insert(?MODULE,Session),
                            Sid
		    end
	    end,
    {reply,NewId,undefined};

handle_call({get_session_data,Sid},_From,_State) ->
    Data = case ets:lookup(?MODULE, Sid) of
	       [S] ->
		   S#erails_session.data;
	       [] ->
		   []
	   end,
    {reply,Data,undefined};

handle_call({set_session_data,Sid,Key,Value},_From,_State) ->
    Data = case ets:lookup(?MODULE,Sid) of
	       [S] ->
		   S#erails_session.data;
	       [] -> []
	   end,
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    Rest = proplists:delete(Key,Data),
		    [{Key,Value}|Rest];
		false ->
		    [{Key,Value}|Data]
	    end,
    
    ets:insert(?MODULE,#erails_session{sid=Sid,data=Data1,ttl=0}),

    {reply,ok,undefined};


handle_call({delete_session,Sid},_From,_State) ->
    ets:delete(?MODULE,Sid),
    {reply,ok,undefined};


handle_call({remove_session_data,Sid,Key},_From,_State) ->
    Data = case ets:lookup(?MODULE,Sid) of
	       [S] ->
		   S#erails_session.data;
	       [] -> []
	   end,
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    proplists:delete(Key,Data);
		false ->
		    Data
	    end,
    
    ets:insert(?MODULE,#erails_session{sid=Sid,data=Data1,ttl=0}),

    {reply,ok,undefined}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

