-module(swan_dict).
-export([get_keywords_utf8/0,
         get_keywords_unicode/0,
         new_table_unicode/1,
         delete_table_unicode/1,
         dump_table_unicode/1]).

-define(KEYWORD_UTF8, "keyword.utf8").

%% @spec get_keywords_utf8() -> [Keyword]
%% @doc read the keyword list from the ?KEYWORD_UTF8 and 
%%      return the keyword list.
get_keywords_utf8() ->
    get_keywords_utf8(?KEYWORD_UTF8).
get_keywords_utf8(KeywordFile) ->
    case file:open(KeywordFile, [read]) of
	{ok, IoDevice} ->
	    Res = read_keywords(IoDevice, []),
	    lists:reverse(Res);
	{error, _Reason} ->
	    []       
    end.
%% Internal APIs
read_keywords(IoDevice, AccIn) ->
    case io:get_line(IoDevice, "") of  %% erl5.7.2 doesn't include file:read_line(IoDevice)
	eof ->
	    AccIn;
	{error, _Reason} ->
	    AccIn;
	Line ->
	    [WordUtf8] = string:tokens(Line, "\n"),
	    read_keywords(IoDevice, [list_to_binary(WordUtf8) | AccIn])
    end.

get_keywords_unicode() ->
    Utf8List = get_keywords_utf8(),
    UnicodeList = lists:foldl(fun(Word, AccIn) ->
                                  [unicode:characters_to_list(Word) | AccIn]
                              end, [], Utf8List),
    lists:reverse(UnicodeList).

new_table_unicode(Name) ->
    UnicodeList = get_keywords_unicode(),
    TableId = ets:new(Name, [ordered_set]),
    lists:foreach(fun(Word) -> 
                      make_index(TableId, Word, length(Word) - 1)
                  end, UnicodeList),
    TableId.
%% Internal API:
%% the ets index include two type data:
%% a. {KeyWord, true}
%% b. {IndexWord, Length:integer()}
%%
%% [{[20339],1},
%%  {[20339,33021],true},
%%  {[23612],1},
%%  {[23612,24247],true},
%%  {[25968],1},
%%  {[25968,30721],2},
%%  {[25968,30721,21333],3},
%%  {[25968,30721,21333,21453],true},
%%  {[32034],1},
%%  {[32034,23612],true},
%%  ....]
make_index(TableId, Word, 0) ->
    ets:insert(TableId, {Word, true});
make_index(TableId, Word, Len) ->
    SubLen = length(Word) - Len,
    SubKey = lists:sublist(Word, SubLen),
    case ets:lookup(TableId, SubKey) of
	[{SubKey, true}] ->
	    %% have insert the k/v pair, just skipped.
	    make_index(TableId, Word, Len - 1);
	_ ->
	    ets:insert(TableId, {SubKey, SubLen}),
	    make_index(TableId, Word, Len - 1)
    end.
    
delete_table_unicode(TableId) ->
    ets:delete(TableId).

dump_table_unicode(TableId) ->
    ets:tab2list(TableId).
