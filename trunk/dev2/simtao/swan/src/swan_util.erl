-module(swan_util).
-include("swan.hrl").
-export([parse_host_and_referer/1,
	 iconv/2, iconv/3,
         convert_to_utf8/2,
	 utf8_to_unicode/1,
	 unicode_to_utf8/1]).

-define(ICONV_UTF8, "utf-8").
-define(ICONV_GB2312, "gb2312").
-define(ICONV_GBK, "gbk").

%% @spec parse_host_and_referer(Req) -> {Host, Referer}
%%   Host = string() | undefined
%%   Referer = string() | undefined
%% @doc parse the 'Host' and 'Referer' in the mochiweb headers.
%%
%% E.g. 
%%   {"192.168.0.12:8000", "http://192.168.0.12:8000/test/utf8"}
parse_host_and_referer(Req) ->
    Headers = Req:get(headers),
    HeadersList = mochiweb_headers:to_list(Headers),
    Host = proplists:get_value('Host', HeadersList),
    Referer = proplists:get_value('Referer', HeadersList),
    {Host, Referer}.


%% encode convert helper APIs:
iconv(CD, Data) ->
    iconv(CD, Data, []).
iconv(CD, Data, Default) ->
    case iconv:conv(CD, Data) of
	{ok, DataConverted} ->
	    DataConverted;
	_ ->
	    Default
    end.

%% @spec convert_to_utf8(IoList, Charset) -> Utf8IoList
convert_to_utf8(IoList, ?CHARSET_UTF8) ->
    IoList;
convert_to_utf8(IoList, ?CHARSET_GB2312) ->
    {ok, CD} = iconv:open(?ICONV_UTF8, ?ICONV_GB2312),
    Text = lists:foldl(fun(Item, AccIn) ->
                           [iconv(CD, Item) | AccIn]
                       end, [], IoList),
    iconv:close(CD),
    lists:reverse(Text);
convert_to_utf8(IoList, ?CHARSET_GBK) ->
    {ok, CD} = iconv:open(?ICONV_UTF8, ?ICONV_GBK),
    Text = lists:foldl(fun(Item, AccIn) ->
                           [iconv(CD, Item) | AccIn]
                       end, [], IoList),
    iconv:close(CD),
    lists:reverse(Text);
convert_to_utf8(IoList, ?CHARSET_UNDEFINED) ->
    IoList.


%% @spec utf8_to_unicode(Utf8IoList) -> UnicodeList
%% @doc convert a utf8 binary to unicode list.
utf8_to_unicode(Utf8IoList) ->
    unicode:characters_to_list(Utf8IoList).

%% @spec unicode_to_utf8(UnicodeList) -> Binary
%% @doc convert a unicode list to utf8 binary
unicode_to_utf8(UnicodeList) ->
    unicode:characters_to_binary(UnicodeList).
