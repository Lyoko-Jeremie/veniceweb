-module(woomsg_datetime).
-export([get_datetime/0, 
         get_date/0,
         get_time/0,
         get_datetime_string/0,
         get_date_string/0,
         get_time_string/0,
         get_date_from_datetime/1,
	 get_time_from_datetime/1,
         get_seconds_since_datetime/1,
         get_fmt_since_datetime/1,
         get_fmt_since_datetime_string/1,
	 is_same_date/2,
         compare_datetime/2]).

%% 时间格式定义:
%%
%% date格式:
%% {Year, Month, Day}
%%
%% Time格式:
%% {Hour, Minute, Second}
%%
%% datetime格式:
%% {{Year, Month, Day},{Hour, Minute, Second}}
%%

%% 获取当前的datetime
get_datetime() ->
    calendar:local_time().

%% 获取当前的date
get_date() ->
    erlang:date().

%% 获取当前的time
get_time() ->
    erlang:now().

get_datetime_string() ->
    {{Y, Month, D},{H, Minute, S}} = calendar:local_time(),
    integer_to_list(Y) ++ integer_to_list(Month) ++ integer_to_list(D) ++ integer_to_list(H) ++ integer_to_list(Minute) ++ integer_to_list(S).

get_date_string() ->
    {Y, M, D} = erlang:date(),
    integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D).  

get_time_string() ->
    {H, M, S} = erlang:now(),
    integer_to_list(H) ++ integer_to_list(M) ++ integer_to_list(S).

get_date_from_datetime(Datetime) ->
    {Date, _Time} = Datetime,
    Date.

get_time_from_datetime(Datetime) ->
    {_Date, Time} = Datetime,
    Time.

%% 返回Datetime距离当前多少秒
get_seconds_since_datetime(Datetime) ->
    NowSec = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DatetimeSec = calendar:datetime_to_gregorian_seconds(Datetime),
    NowSec - DatetimeSec.

%% 返回string()形式的日期
get_fmt_since_datetime_string(Datetime) ->
    Diff = get_seconds_since_datetime(Datetime),
    Res = 
        if
            Diff < 60 ->
	        io_lib:format("~p秒前",[Diff]);
 	    Diff < 3600 ->
	        io_lib:format("~p分钟前", [round(Diff/60)]);
	    Diff < 86400 ->
	        io_lib:format("~p小时前", [round(Diff/3600)]);
	    true ->
	        io_lib:format("~p天前", [round(Diff/86400)])
        end,
    Res.

%% 返回
%%   {second_ago, Val}
%%   {minute_ago, Val}
%%   {hour_ago, Val}
%%   {day_ago, Val}
get_fmt_since_datetime(Datetime) ->
    Diff = get_seconds_since_datetime(Datetime),
    {Type, Val} = 
        if
            Diff < 60 ->
	        {second_ago, Diff};
 	    Diff < 3600 ->
	        {minute_ago, round(Diff/60)};
	    Diff < 86400 ->
	        {hour_ago, round(Diff/3600)};
	    true ->
	        {day_ago, round(Diff/86400)}
        end,
    {Type, integer_to_list(Val)}.

%% 比较两个时间是同一天(只计算date不计算time)
is_same_date(Datetime1, Datetime2) ->
   {{Y1, M1, D1}, _} = Datetime1,
   case Datetime2 of
       {{Y1, M1, D1}, _} ->
           true;
       _ ->
	   false
   end.

%% 比较Datetime1和Datetime2的时间:
%% 如果Datetime1在Datetime2之后: big
%% 如果Datetime1在Datetime2之前: small 
%% 如果Datetime1和Datetime2相等: equal   
compare_datetime(Datetime1, Datetime2) ->
    Sec1 = get_seconds_since_datetime(Datetime1),
    Sec2 = get_seconds_since_datetime(Datetime2),
    Delta = Sec1 - Sec2,
    case Delta =:= 0 of
	true ->
	    equal;
        false when Delta > 0 ->
	    small;
        false when Delta < 0 ->
	    big
    end.
