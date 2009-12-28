-module(woomsg_util).
-export([list_to_integer/1, list_to_integer/2]).

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

