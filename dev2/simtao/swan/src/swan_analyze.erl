-module(swan_analyze).
-export([handle_get/1, handle_post/1]).

handle_get(Req) ->
    Data = mochiweb_html:to_html(
	   {"html", [], [
	   {"head", [], [
	     {"meta", [{"http-equiv", "Content-Type"}, 
		       {"content", "text/html; charset=utf-8"}], []},
	     {"title", [], ["页面分析"]}]},
	   {"body", [{"style", "background-color: #E2F2FB;"}], [
	     {"div", [{"class", "page"}], [
	       {"div", [{"id", "header"}], []},
	       {"div", [{"id", "menucontainer"}], []},
               {"div", [{"id", "main"}], [
                 {"div", [{"style", "font-size: 16px; font-weight: bold; margin-bottom: 10px;"}], 
                          ["Simtao Keywords Service 测试"]}, 
	         {"div", [{"id", "url"}], [
		   {"form", [{"method", "post"}, {"action", "/service/analyze"}], [
		     "输入URL:",
                     {"input", [{"type", "text"}, {"name", "url"}], []},
                     {"input", [{"type", "submit"}, {"value", "发送"}], []}]}]},
                 {"div", [{"id", "res"}], []}]},
               {"div", [{"id", "footer"}], []}]}]}]}),
    Req:respond({200, [{"Content-Type", "text/html"}], Data}).

handle_post(Req) ->
    [{"url", Url}] = Req:parse_post(),
    {Charset, TextUtf8, KeywordsUnicode, Runtime, WallClock} = swan_http:process_url(Url),

    Data = mochiweb_html:to_html(
	   {"html", [], [
	   {"head", [], [
	     {"meta", [{"http-equiv", "Content-Type"}, 
		       {"content", "text/html; charset=utf-8"}], []},
	     {"title", [], ["页面分析"]}]},
	   {"body", [{"style", "background-color: #E2F2FB;"}], [
	     {"div", [{"class", "page"}], [
	       {"div", [{"id", "header"}], []},
	       {"div", [{"id", "menucontainer"}], []},
               {"div", [{"id", "main"}], [
                 {"div", [{"style", "font-size: 16px; font-weight: bold; margin-bottom: 10px;"}], 
                          ["Simtao Keywords Service 测试"]},
	         {"div", [{"id", "url"}], [
		   {"form", [{"method", "post"}, {"action", "/service/analyze"}], [
		     "输入URL:",
                     {"input", [{"type", "text"}, {"name", "url"}], []},
                     {"input", [{"type", "submit"}, {"value", "发送"}], []}]}]},
                 {"div", [{"id", "res"}], [
                   {"div", [{"style", "background-color: #CCE9EF; border:1px solid #0B3A5E; margin: 5px 0px"}],
                           ["Url: ", Url]},
                   {"div", [{"style", "background-color: #CCE9EF; border:1px solid #0B3A5E; margin: 5px 0px;"}], 
                           ["Charset: ", atom_to_list(Charset)]},
                   {"div", [{"style", "background-color: #CCE9EF; border:1px solid #0B3A5E; margin: 5px 0px"}], 
		           ["Keywords: ", format_keywords(KeywordsUnicode)]},
		   {"div", [{"style", "background-color: #CCE9EF; border:1px solid #0B3A5E; margin: 5px 0px;"}], 
		           ["Runtime(占用的CPU时间): ", io_lib:format("~p 毫秒~n", [Runtime])]},
                   {"div", [{"style", "background-color: #CCE9EF; border:1px solid #0B3A5E; margin: 5px 0px"}], 
		           ["WallClock(代码执行时间): ", io_lib:format("~p 毫秒~n", [WallClock])]},
                   {"div", [{"style", "background-color: #CCE9EF; border:1px solid #0B3A5E; margin: 5px 0px;"}], 
		           ["Text: ", TextUtf8]}]}]},
               {"div", [{"id", "footer"}], []}]}]}]}),
    Req:respond({200, [{"Content-Type", "text/html"}], Data}).

%% Internal APIs:
format_keywords(KeywordsUnicode) ->
    Res = lists:foldl(fun({Key, Count}, AccIn) ->
			  TmpData = ["(", swan_util:unicode_to_utf8(Key), ", ",integer_to_list(Count), ")"],
                          [", ", TmpData | AccIn]
                      end, [], KeywordsUnicode),
    lists:reverse(Res).
