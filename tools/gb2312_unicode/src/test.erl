-module(test).
-compile(export_all).

%% show the gb2312 & unicode mapping table
show_gb2312_table(Count) ->
    show_gb2312_table("GB2312.TXT", Count).
show_gb2312_table(TableFile, Count) ->
    case file:open(TableFile, [read]) of
	{ok, IoDevice} ->
	    case display_lines(IoDevice, Count) of
		{ok, NewCount} ->
		    file:close(IoDevice),
		    {ok, NewCount};
		{eof, NewCount} ->
		    file:close(IoDevice),
		    {ok, NewCount};
		{error, NewCount} ->
		    file:close(IoDevice),
		    {error, NewCount}
	    end;
	{error, _Reason} ->
	    {error, Count}      
    end.

%% Internal
display_lines(IoDevice, Count) when Count > 0 ->
    case io:get_line(IoDevice, "") of
	eof ->
	    {eof, Count};
	{error, _Reason} ->
	    {error, Count};
	Line ->
	    case string:tokens(Line, "\t") of
		[GB2312, Tail] ->
		    [Unicode] = string:tokens(Tail, "\n"),
		    io:format("GB2312:~p, Unicode:~p~n", [GB2312, Unicode]),
		    display_lines(IoDevice, Count - 1);
		_ ->
		    {error, Count}       
            end
    end;
display_lines(_IoDevice, Count) ->
    {ok, Count}.
    

load_gb2312_table() ->
    load_gb2312_table("GB2312.TXT").
load_gb2312_table(TableFile) ->
    Dict = dict:new(),
    case file:open(TableFile, [read]) of
	{ok, IoDevice} ->
	    case parse_lines(IoDevice, Dict) of
		{eof, NewDict} ->
		    file:close(IoDevice),
		    {ok, NewDict};
		{error, NewDict} ->
		    file:close(IoDevice),
		    {error, NewDict}
	    end;
	{error, _Reason} ->
	    {error, Dict}       
    end.

%% Internal
parse_lines(IoDevice, Dict) ->
    case io:get_line(IoDevice, "") of  %% erl5.7.2 doesn't include file:read_line(IoDevice)
	eof ->
	    {eof, Dict};
	{error, _Reason} ->
	    {error, Dict};
	Line ->
	    case string:tokens(Line, "\t") of
		[GB2312, Tail] ->
		    [Unicode] = string:tokens(Tail, "\n"),
		    Dict1 = dict:store(GB2312, Unicode, Dict),
		    Dict2 = dict:store(Unicode, GB2312, Dict1),
		    parse_lines(IoDevice, Dict2);
		_ ->
		    {error, Dict}       
            end
    end.


%% "0x674E" ->  26446
%% "0X674E" ->  26446
%% "674E"   ->  26446
%%
%% Note:
%% the chars in HexStr must uppercase: A - E. (doesn't support lowercase a- e)
hex_to_integer(HexStr) ->
    %% remove the prefix first
    HexStr1 = case string:substr(HexStr, 1, 2) of
		  "0x" ->
		      string:substr(HexStr, 3);
		  "0X" ->
		      string:substr(HexStr, 3);
		  _ ->
		      HexStr
              end,
    {_, IntRes} = lists:foldr(fun(Elem, AccIn) ->
		                 {Mul, Data} = AccIn,
				 NewData = Data + hexchar_to_integer(Elem) * Mul,
				 {Mul*16, NewData}
                              end, {1, 0}, HexStr1),
    IntRes.


integer_to_hex(Integer) ->
    integer_to_hex(Integer, "").
%% Prefix: "0X"
integer_to_hex(Integer, Prefix) ->
    BinList = integer_to_binarylist(Integer, ""),
    BinList1 = lists:reverse(BinList), %% reverse here!
    Prefix ++ process_binarylist(BinList1, 1, "").

%% Internal
process_binarylist(BinList, Index, AccIn) ->
    Len = erlang:length(BinList),
    case Index + 4 =< Len of
	true ->
	    [A, B, C, D] = lists:sublist(BinList, Index, 4),
	    IntRes = A + B*2 + C*4 + D*8,
	    process_binarylist(BinList, Index + 4, integer_to_hexchar(IntRes) ++ AccIn);
	false ->
	    IntRes = case lists:sublist(BinList, Index, 4 ) of
		          [A, B, C, D] ->
		              A + B*2 + C*4 + D*8;
		          [A, B, C] ->
		              A + B*2 + C*4;
		          [A, B] ->
		              A + B*2;
		          [A] ->
		              A
	             end,
	    integer_to_hexchar(IntRes) ++ AccIn
    end.

%% Internal
%% Int 0 - 15 -> Char [$0 - $9 | $A - $F ]
integer_to_hexchar(IntRes) ->
    case (IntRes >= 0) and (IntRes =< 9) of
        true ->
            %% 0 - 9
	    integer_to_list(IntRes);
	false ->	
	    case (IntRes >= 10) and (IntRes =< 15) of
	    true ->
	        %% 10 - 15 -> A - F
                [($A + (IntRes - 10))]
	    end
    end.

%% Internal
%% Char [$0 - $9 | $A - $F ] -> Int 0 - 15
hexchar_to_integer(HexRes) ->
    case (HexRes - $0 >= 0) and (HexRes - $0 =< 9) of
        true ->
            %% 0 - 9
            HexRes - $0;
        false ->
            case (HexRes - $A >= 0) and (HexRes - $A =< 5) of
                true ->
                    %% A - F
                    HexRes - $A + 10
            end
    end.
    
%% Internal
integer_to_binarylist(Integer, Data) when Integer > 1 ->
    integer_to_binarylist((Integer div 2), [(Integer rem 2) | Data]);
integer_to_binarylist(Integer, Data) when Integer =:= 1 ->
    [ Integer | Data].
    


    
    
    
    
    
    
    
    
    
    
