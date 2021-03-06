-module(swan_http).
-include("swan.hrl").
-export([process_url/1]).

-define(HEADERS, [{"User-Agent", "simtao keywords spider"},
                  {"Accept", "text/xml"}]).

%% Bug1# Active
%% this is a bug of mochiweb_html:parse/1
%% it can't right parse the code without the quotes in the attributes: 
%% Repro:
%% <script type=text/javascript> ... </script>    - parse error
%% <script type="text/javascript"> ... </script>  - parse right
%%

%% Bug2# Fixed
%% Body: "<script>document.location.href='http://news.163.com/special/special.html'</script>"
%% HtmoNode: {<<"script">>,[],[<<"document.location.href='http://news.163.com/special/special.html'">>]}
%% Repro:
%% access a removed url, just return the redirect script, the script caused parse_charset/1 crashed.
%%
%% Fix:
%% has fixed this issue.


%% @doc this is a mini-spider, fetch the page of the Url, and return the 
%%      'charset' and 'utf-8 encoding text'
%% 
%% steps in this function:
%% <1> parse the data to html_node()
%% <2> parse the charset(utf-8, gb2312 ... )
%% <3> parse the pure-text
%% <4> convert the pure-text to utf8
%% <5> convert the utf8-pure-text(the results of <4>) to unicode list
%% <6> 'token' the unicode list
%% <7> merge the 'token' results and got a unicode keyword list.
process_url(Url) ->
    case http:request(get, {Url, ?HEADERS}, [], [{body_format, string}]) of
	{ok, {{_Version, 200, _Reason}, _Headers, Body}} ->

            %% Note: 
            %% TODO: remove the perf counter in the product env.
	    %%
	    %% Install a perf counter for debug 
            statistics(runtime),
            statistics(wall_clock),
	    HtmlNode = mochiweb_html:parse(Body),
	    Charset = parse_charset(HtmlNode),    
	    TextOri = parse_html(HtmlNode, []),   
	    TextUtf8 = swan_util:convert_to_utf8(TextOri, Charset),
            TextUnicode = swan_util:utf8_to_unicode(TextUtf8),
            KeywordsUnicode  = swan_token:tokens(TextUnicode),
            MergedKeywordsUnicode = swan_token:merge(KeywordsUnicode),

	    {_, Runtime} = statistics(runtime),
            {_, WallClock} = statistics(wall_clock),

	    {Charset, TextUtf8, MergedKeywordsUnicode, Runtime, WallClock};
	{ok, {{_Version, 404, _Reason}, _Headers, _Body}} ->
	    io:format("http:request 404 not found: ~p~n", [Url]),
	    {undefined, "undefined", [], undefined, undefined};
	Other ->
            io:format("http:request unknown return: ~p~n", [Other]),
	    {undefined, "undefined", [], undefined, undefined}
    end.


%% Internal APIs:
%% @spec parse_charset(HtmlNode) -> Charset
%%   Charset = 'utf-8' | gb2312 | undefined
%%   HtmlNode = html_node(), the return value of mochiweb_html:parse/1
%% @doc parse the charset from html_node().
%%
%% E.g. 
%% <1>
%% html document:
%% <meta content="text/html; charset=utf-8" http-equiv="Content-Type">
%% html_node():
%% {<<"meta">>, [{<<"content">>, <<"text/html; charset=utf-8">>}, 
%%               {<<"http-equiv">>, <<"Content-Type">>}], []}
%% <2>
%% html document:
%% <meta charset="gbk">
%% html_node():
%% {<<"meta">>, [{<<"charset">>, <<"gbk">>}], []}
parse_charset({_Name, _Attrs, []}) ->
    undefined;
parse_charset({<<"HTML">>, Attrs, ChildNodes}) ->
    parse_charset({<<"html">>, Attrs, ChildNodes});
parse_charset({<<"html">>, _Attrs, ChildNodes}) ->
    case lists:keysearch(<<"head">>, 1, ChildNodes) of
	{value, HeadNode} ->
	    parse_head(HeadNode);
	_ ->
	    case lists:keysearch(<<"HEAD">>, 1, ChildNodes) of
		{value, HeadNode} ->
		    parse_head(HeadNode);
		_ ->
		    undefined
            end
    end;
%% sometimes, 'html' is the not root node.
%% E.g.{<<"!doctype">>, _, [{<<"html">>, _, _}]}.
%%
%% fix bug#2.
%%
parse_charset(Binary) when is_binary(Binary) ->
    undefined;
parse_charset({_Name, _Attrs, []}) ->
    undefined;
parse_charset({_Name, _Attrs, ChildNodes}) ->
    parse_charset(hd(ChildNodes)).

parse_head({<<"HEAD">>, Attrs, ChildNodes}) ->
    parse_head({<<"head">>, Attrs, ChildNodes});
parse_head({<<"head">>, _Attrs, []}) ->
    undefined;
parse_head({<<"head">>, _Attrs, ChildNodes}) ->
    Res = lists:foldl(fun(Node, AccIn) ->
		          case AccIn of
			      undefined ->
				  %% not found, go on search :)
				  parse_attr(Node);
			      Any ->
				  Any
                          end
                      end, undefined, ChildNodes),
    Res.
parse_attr({<<"META">>, Attrs, ChildNodes}) ->
    parse_attr({<<"meta">>, Attrs, ChildNodes});
parse_attr({<<"meta">>, [], _ChildNodes}) ->
    undefined;
parse_attr({<<"meta">>, Attrs, _ChildNodes})  ->
    case proplists:get_value(<<"http-equiv">>, Attrs) of
	undefined ->
	    case proplists:get_value(<<"charset">>, Attrs) of
		undefined ->
		    ?CHARSET_UNDEFINED;
		<<"utf-8">> ->
		    ?CHARSET_UTF8;
		<<"UTF-8">> ->
		    ?CHARSET_UTF8;
		<<"Utf-8">> ->
		    ?CHARSET_UTF8;
		<<"gb2312">> ->
		    ?CHARSET_GB2312;
		<<"GB2312">> ->
		    ?CHARSET_GB2312;
		<<"Gb2312">> ->
		    ?CHARSET_GB2312;
		<<"gbk">> ->
		    ?CHARSET_GBK;
		<<"GBK">> ->
		    ?CHARSET_GBK;
		<<"gbk">> ->
		    ?CHARSET_GBK;
		_ ->
		    ?CHARSET_UNDEFINED
            end;
	_ ->
	    case proplists:get_value(<<"content">>, Attrs) of
		undefined ->
		    ?CHARSET_UNDEFINED;
		<<"text/html; charset=utf-8">> ->
		    ?CHARSET_UTF8;
		<<"text/html; charset=UTF-8">> ->
		    ?CHARSET_UTF8;
		<<"text/html; charset=Utf-8">> ->
		    ?CHARSET_UTF8;
		<<"text/html; charset=gb2312">> ->
		    ?CHARSET_GB2312;
		<<"text/html; charset=GB2312">> ->
		    ?CHARSET_GB2312;
		<<"text/html; charset=Gb2312">> ->
		    ?CHARSET_GB2312;
		<<"text/html; charset=gbk">> ->
		    ?CHARSET_GBK;
		<<"text/html; charset=GBK">> ->
		    ?CHARSET_GBK;
		<<"text/html; charset=Gbk">> ->
		    ?CHARSET_GBK;
		_ ->
		    ?CHARSET_UNDEFINED
	    end
    end;
parse_attr(_Any)->
    undefined.


%% Internal APIs:
%% @doc 解析html_node()的时候使用: "深度优先搜索遍历算法"
%%
parse_html({_Name, _Attrs, []}, []) ->
    [];
parse_html({_Name, _Attrs, ChildNodes}, []) ->
    Res = lists:foldl(fun(Node, AccInSub) ->
		          parse_node(Node, AccInSub)
                      end, [], ChildNodes),
    lists:reverse(Res).

%% skip the javascript and css code:
%% <1> <style type="text/css"> ... </style>
%% <2> <script type="text/javascript"> ... </script>
%% <3> <link type="text/css"> ... </link>
%%
%% <4> <textarea> ... </textarea> is this ok?
parse_node(Node, AccIn) when is_binary(Node) ->
    [Node | AccIn];
parse_node({<<"script">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"SCRIPT">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"Script">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"style">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"STYLE">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"Style">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"link">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"LINK">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"Link">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"textarea">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"TEXTAREA">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({<<"Textarea">>, _Attrs, _ChildNodes}, AccIn) ->
    AccIn;
parse_node({_Name, _Attrs, []}, AccIn) ->
    AccIn;
parse_node({_Name, _Attrs, ChildNodes}, AccIn) ->
    Res = lists:foldl(fun(Node, AccInSub) ->
		          parse_node(Node, AccInSub)
                      end, AccIn, ChildNodes),
    Res;
parse_node(_Node, AccIn) ->
    AccIn.
