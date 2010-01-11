-module(woomsg_guid).
-export([get_random_string/1,
         get_comment_guid/0,
         get_image_guid/0,
         get_session_guid/0,
         get_path_guid/0,
         get_guid_by_datetime/1,
         get_guid_by_date/1,
         get_guid_by_time/1]).

-define(COMMENT_GUID_SUFFIX, 6).
-define(IMAGE_GUID_SUFFIX, 12).
-define(SESSION_GUID_SUFFIX, 6).
-define(PATH_GUID_SUFFIX, 6).

%%
%% 注意:
%% 在调用这里面的方法的时候保证crtypto:start/0已经被调用.
%%

%% 功能:
%%  产生一个指定长度的随机string
%%  (在调用这个方法之前, 必须先crypto:start/0)
%%
%% @ Len:integer()
%%
%% @return Val:string() 
get_random_string(Len) ->
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

%% 产生评论的GUID
get_comment_guid() ->
   get_guid_by_time(?COMMENT_GUID_SUFFIX).

%% 产生图片的GUID
get_image_guid() ->
   get_guid_by_time(?IMAGE_GUID_SUFFIX).

%% 产成Session的GUID
get_session_guid() ->
   get_guid_by_date(?SESSION_GUID_SUFFIX).

%% 产生路径的GUID
get_path_guid() ->
   get_guid_by_date(?PATH_GUID_SUFFIX).

%% 三种GUID生成方式
get_guid_by_datetime(SuffixLen) ->
   woomsg_datetime:get_datetime_string() ++ get_random_string(SuffixLen).

get_guid_by_date(SuffixLen) ->
    woomsg_datetime:get_date_string() ++ get_random_string(SuffixLen).

get_guid_by_time(SuffixLen) ->
    woomsg_datetime:get_time_string() ++ get_random_string(SuffixLen).


