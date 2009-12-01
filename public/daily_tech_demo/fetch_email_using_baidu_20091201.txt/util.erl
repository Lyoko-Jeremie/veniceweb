-module(util).
-compile(export_all).

parse_email(Subject) ->
    parse_email(Subject, 0, []).

parse_email(Subject, Offset, Acc) ->
    RE = "\\w+@\\w+\\.\\w+", %% 匹配Email的正则表达式:
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

process_html(Html) ->
    Body = parse_body(Html),
    remove_html_tag(Body).

parse_body(Subject) ->
    case re:run(Subject, "<body.*?</body>", [dotall, {capture, first, list}, {offset, 0}]) of
	nomatch ->
	    "";
	{match, [Match]} ->
	    Match
    end.

remove_html_tag(Data) ->
    remove_html_tag(Data, 0).

remove_html_tag(Data, Offset) ->
    case re:run(Data, "<.*?>", [dotall, {capture, first, index}, {offset, Offset}]) of
	nomatch ->
	    Data;
	{match, [{Index, _Len}]}->
	    %% 注意: Offset = Index, 以为长度为Len的数据已经被替换成[]
	    remove_html_tag(re:replace(Data, "<.*?>", "", [dotall,{return, list}]), Index) 
    end.
