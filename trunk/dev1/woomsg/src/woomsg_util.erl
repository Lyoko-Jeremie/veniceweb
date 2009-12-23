-module(woomsg_util).
-export([get_random_key/1,
	 get_current_time/0,
	 get_guid/0,
	 list_to_integer/1, list_to_integer/2]).

%%
%% 注意:
%% 在调用这里面的方法的时候保证crtypto:start/0已经被调用.
%%

%% 功能:
%%  产生一个指定长度的随即Key
%%  (在调用这个方法之前, 必须先crypto:start/0)
%%
%% @Len:integer()
%%
%% @return Key:string() 
get_random_key(Len) ->
    Chars =
	{$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,
	 $w,$x,$y,$z,
	 $A,$B,$C,$D,$E,$F,$G,$H,$I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,
	 $W,$X,$Y,$Z,
	 $1,$2,$3,$4,$5,$6,$7,$8,$9,$0},
    Res = lists:map(
	    fun(_) ->
		    Idx = crypto:rand_uniform(1, 62),
		    element(Idx, Chars)
	    end, lists:seq(1, Len)),
    Res.

get_current_time() ->
    case erlang:now() of
	{N1, N2, N3} ->
	    integer_to_list(N1) ++ integer_to_list(N2) ++ integer_to_list(N3);
	_ ->
	    ""
    end.

get_guid() ->
    woomsg_util:get_current_time() ++ woomsg_util:get_random_key(12).


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

