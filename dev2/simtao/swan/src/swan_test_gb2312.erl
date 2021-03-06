-module(swan_test_gb2312).
-include("swan.hrl").
-export([handle_get/1]).

handle_get(Req) ->
    {ok, CD} = iconv:open("gb2312", "utf-8"),
    TitleGB2312 = swan_util:iconv(CD, "GB2312-测试"),
    H2GB2312 = swan_util:iconv(CD, "效果演示"),
    P1GB2312 = swan_util:iconv(CD, "以下的短文摘录自天极网，版权属天极网所有，在此仅作示例使用"),
    P2GB2312 = swan_util:iconv(CD, "纵观近年来高端单反市场产品走势，像素的升级竞赛，以及全画幅产品的逐渐普及，可谓备受诸多高端用户关注的焦点。而此次我们特意将目前佳能、尼康和索尼三家影像大厂的三款顶级超高像素全画幅单反进行横向对比，分别是佳能EOS-1Ds Mark III、尼康D3X以及索尼a900。这三款相机堪称目前顶级全幅数码单反重量级的大杀器。而在对比之前，先向一些入新手用户介绍一下何为全画幅单反，以及全画幅的意义。"),
    P3GB2312 = swan_util:iconv(CD, "除了都是全幅单反外，此次横评的三款机型更是拥有2000万以上超高像素的机型，遥遥领先于目前业内单反像素的主流水准。一些普通用户可能会觉得，超高像素并没有太大的实际意义。不过对于摄影师以及高端发烧友而言，超高像素为图像的后期处理，以及高画质的输出提供很多便利。因此就莫种角度来看，超高像素也象征着产品的档次和品质。"),
     Data = mochiweb_html:to_html(
	   {"html", [], [
	   {"head", [], [
	     {"meta", [{"http-equiv", "Content-Type"}, 
		       {"content", "text/html; charset=gb2312"}], []},
	     {"title", [], [TitleGB2312]}]},
	   {"body", [], [
	     {"div", [{"class", "page"}], [
	       {"div", [{"id", "header"}], []},
	       {"div", [{"id", "menucontainer"}], []},
               {"div", [{"id", "main"}], [
		 {"h2", [], [H2GB2312]},
                 {"p", [], [P1GB2312]},
		 {"p", [], [P2GB2312]},
                 {"p", [], [P3GB2312]},
                 {"script", [{"charset", "gb2312"}, {"type", "text/javascript"},
			     {"src", ?API_KEYWORDS ++ "?code=gb2312code"}], []}]},
               {"div", [{"id", "footer"}], []}]}]}]}),
    iconv:close(CD),
    Req:respond({200, [{"Content-Type", "text/html"}], Data}).
