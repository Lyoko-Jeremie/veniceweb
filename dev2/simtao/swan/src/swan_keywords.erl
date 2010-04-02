-module(swan_keywords).
-include("swan.hrl").
-export([handle_get/1]).

handle_get(Req) ->
    case Req:parse_qs() of
	[{"code", Code}] ->
	    {Host, Referer} = swan_util:parse_host_and_referer(Req),
            {Charset, _TextUtf8, KeywordsUnicode, _Runtime, _WallClock} = swan_http:process_url(Referer),
	    Data = ["var code=", Code, ";\n",
		   "var host=", Host, ";\n",
		   "var referer=", Referer, ";\n",
		   "var keywordList=[", format_keywords(KeywordsUnicode, Charset), "];\n"],
            Req:respond({200, [{"Content-Type", "text/plain"}], Data});
	_ ->
	    Data = "api-request-error;",
            Req:respond({200, [{"Content-Type", "text/plain"}], Data})	    
    end.

%% Internal APIs:
%% ["尼康","佳能","索尼","数码单反","胶卷"]
format_keywords(KeywordsUnicode, ?CHARSET_GB2312) ->
    {ok, CD} = iconv:open("gb2312", "utf-8"),
    TitleGB2312 = swan_util:iconv(CD, "GB2312-测试"),
    Res = lists:foldl(fun({Key, _Count}, AccIn) ->
			  Utf8Key = swan_util:unicode_to_utf8(Key),
			  TmpData = ["\"", swan_util:iconv(CD, Utf8Key), "\""],
                          [",", TmpData | AccIn]
                      end, [], KeywordsUnicode),
    iconv:close(CD),
    lists:reverse(Res);
format_keywords(KeywordsUnicode, ?CHARSET_UTF8) ->
    Res = lists:foldl(fun({Key, _Count}, AccIn) ->
			  TmpData = ["\"", swan_util:unicode_to_utf8(Key), "\""],
                          [",", TmpData | AccIn]
                      end, [], KeywordsUnicode),
    lists:reverse(Res);
format_keywords(KeywordsUnicode, ?CHARSET_UNDEFINED) ->
    Res = lists:foldl(fun({Key, _Count}, AccIn) ->
			  TmpData = ["\"", swan_util:unicode_to_utf8(Key), "\""],
                          [",", TmpData | AccIn]
                      end, [], KeywordsUnicode),
    lists:reverse(Res).
