-module(util).
-compile(export_all).

parse_email(Subject) ->
    parse_email(Subject, 0, []).

%%
%% Email format:
%% liqiang@gmail.com         Support
%% liqiang_123@gmail.com     Support
%% liqiang_123@gmail.com.cn  (Not Support)
parse_email(Subject, Offset, Acc) ->
    %% RE = "\\h\\w+@\\w+\\.\\w+",
    RE = "[a-z|A-Z|1-9]\\w+@\\w+\\.\\w+[a-z|A-Z|1-9]", 
    case re:run(Subject, RE, [dotall, {capture, first, index}, {offset, Offset}]) of
	nomatch ->
	    Acc;
	{match, [{Index, Len}]} ->
	    case re:run(Subject, RE, [dotall, {capture, first, list}, {offset, Offset} ]) of
		nomatch ->
		    error;
		{match, [Match]} ->
		    parse_email(Subject, Index + Len, [Match|Acc])
            end
	    
    end.

remove_html_tag(Data) ->
    remove_html_tag(Data, 0).

remove_html_tag(Data, Offset) ->
    case re:run(Data, "<.*?>", [dotall, {capture, first, index}, {offset, Offset}]) of
	nomatch ->
	    Data;
	{match, [{Index, _Len}]}->
	    %%
	    remove_html_tag(re:replace(Data, "<.*?>", "", [dotall,{return, list}]), Index) 
    end.
