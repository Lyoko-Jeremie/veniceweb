-module(swan_token).
-export([merge/1, tokens/1]).

-define(MAX_TOKEN_LEN, 4).

%% @spec merge(UnicodeList) -> NewUnicodeList
%% @doc merge the keywords unicode list to a new format(include the count)
%% NewUnicodeList
%%
%% E.g.
%% [{[25968,30721,21333,21453],4},
%%  {[20339,33021],2},
%%  {[25968,30721,30456,26426],1},
%%  {[32034,23612],2},
%%  {[23612,24247],2}]
merge(UnicodeList) ->
    TmpId = ets:new(?MODULE, [set]),
    lists:foreach(fun(Key) ->
		      case ets:lookup(TmpId, Key) of
			  [{Key, Count}] ->
			      ets:insert(TmpId, {Key, Count + 1});
			  _ ->
			      ets:insert(TmpId, {Key, 1})
		      end
		  end, UnicodeList),
    NewUnicodeList = ets:tab2list(TmpId),
    ets:delete(TmpId),
    NewUnicodeList.
    

%% @doc analyze the unicode list, return the keyword list.
tokens(UnicodeList) ->
    tokens(swan_dict_server:get_tableid(), UnicodeList, []).

%% @spec tokens(TableId, UnicodeList, []) -> [Keyword]
%% 
%% E.g.
%% [[20339,33021],
%%  [23612,24247],
%%  [32034,23612],
%%  [20339,33021],
%%  [23612,24247],
%%  [32034,23612],
%%  [25968,30721,21333,21453],
%%  [25968,30721,21333,21453],
%%  [25968,30721,21333,21453],
%%  [25968,30721,21333,21453]]
tokens(_TableId, [] , Acc) ->
    lists:reverse(Acc);
tokens(TableId, [H|T] = UnicodeList, Acc) ->
    FirstKey = [H],
    case ets:lookup(TableId, FirstKey) of
	[{FirstKey, 1}] ->
	    %% start search
            case process_tokens(TableId, 
				lists:sublist(UnicodeList, ?MAX_TOKEN_LEN), 
				?MAX_TOKEN_LEN) of
		undefined ->
		    tokens(TableId, T, Acc);
		FoundKey ->
		    tokens(TableId, 
			   lists:sublist(UnicodeList, 
					 length(FoundKey), 
					 length(UnicodeList) - length(FoundKey)), 
			   [FoundKey | Acc])
	    end;
	_ ->
	    tokens(TableId, T, Acc)
    end.

%% Internal APIs:
process_tokens(_TableId, _UnicodeList, 1) ->
    undefined;
process_tokens(TableId, UnicodeList, Len) ->
    TmpKey = lists:sublist(UnicodeList, Len),
    case ets:lookup(TableId, TmpKey) of
	[{TmpKey, true}] ->
	    TmpKey;
	_ ->
	    process_tokens(TableId, UnicodeList, Len-1)
    end.
