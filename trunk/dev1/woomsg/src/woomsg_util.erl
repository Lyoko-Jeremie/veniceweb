-module(woomsg_util).
-export([list_to_integer/1, list_to_integer/2,
         list_index_prefix/2,
         list_index_suffix/2]).

%% 安全的list_to_integer.
list_to_integer(NumStr) ->
    woomsg_util:list_to_integer(NumStr, undefined).
list_to_integer(NumStr, Default) ->
    case catch erlang:list_to_integer(NumStr) of
        {'EXIT', _} ->
	    Default;
	Num ->
	   Num
    end.


%% 使用string:chr(String, Char) Index | 0 
%% 来代替私有的实现.
%%
%% 实现list中元素的搜索功能, 返回Elem的索引位置. 
%% (用于HTTP路径的分割)
%% Elem是List中的子元素:
%% 如果存在返回索引: Index (从1开始)
%% 不存在返回:       out_of_index  
%%
%% E.g.
%% list_index_num($/, "login/username") -> 6
%% list_index_num($a, "login")          -> out_of_index
%% list_index_num(Elem, List) ->
%%     list_index_num(Elem, List,1).

%% list_index_num(_Elem, [], _N) ->
%%     out_of_index;
%% list_index_num(Elem, [H|T], N) ->
%%     if
%%         Elem =:= H ->
%% 	    N;
%%         true ->
%% 	    list_index_num(Elem, T, N + 1)
%%     end.

%% 根据第一个Elem元素分割List
%% 如果存在, 则返回前缀.
%% 如果不存在, 则返回List.
%%
%% E.g.
%% list_index_prefix($/, "login/username") -> "login"
list_index_prefix(Elem, List) ->
    case string:chr(List, Elem) of
	0 ->
	    List;
	Index ->
	    lists:sublist(List, Index - 1)
    end.

%% 根据第一个Elem元素分割List
%% 如果存在, 则返回后缀.
%% 如果不存在, 则返回List.
%%
%% E.g.
%% list_index_suffix($/, "login/username/xxx/") -> "username/xxx/"
list_index_suffix(Elem, List) ->
    case string:chr(List, Elem) of
        0 ->
	    List;
	Index ->
	    lists:sublist(List, Index + 1, erlang:length(List) - Index)
    end.
