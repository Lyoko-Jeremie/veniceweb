-module(test).
-compile(export_all).

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

display_lines(IoDevice, Count) when Count > 0 ->
    case file:read_line(IoDevice) of
	{ok, Line} ->
	    case string:tokens(Line, "\t") of
		[GB2312, Tail] ->
		    [UTF8] = string:tokens(Tail, "\n"),
		    io:format("GB2312:~p, UTF8:~p~n", [GB2312, UTF8]),
		    display_lines(IoDevice, Count - 1);
		_ ->
		    {error, Count}       
            end;
	eof ->
	    {eof, Count};
	{error, _Reason} ->
	    {error, Count}
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

parse_lines(IoDevice, Dict) ->
    case file:read_line(IoDevice) of
	{ok, Line} ->
	    case string:tokens(Line, "\t") of
		[GB2312, Tail] ->
		    [UTF8] = string:tokens(Tail, "\n"),
		    Dict1 = dict:store(GB2312, UTF8, Dict),
		    Dict2 = dict:store(UTF8, GB2312, Dict1),
		    parse_lines(IoDevice, Dict2);
		_ ->
		    {error, Dict}       
            end;
	eof ->
	    {eof, Dict};
	{error, _Reason} ->
	    {error, Dict}
    end.


    
    
    
    
    
    
    
    
    
    
