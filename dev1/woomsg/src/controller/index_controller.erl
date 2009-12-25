-module(index_controller).
-export([handle_get/2, handle_post/2]).

handle_get(Req, _DocRoot) ->
    Data = index_view:index("love you"),
    Cookie1 = mochiweb_cookies:cookie("ses_username", "aaaaaaasdfasdfasdf", []),
    Cookie = mochiweb_cookies:cookie("ses_session_id", "asdfasdfasdf", []),
    Req:respond({200, [{"Content-Type","text/html"}, Cookie, Cookie1], Data}).

handle_post(_Req, _DocRoot) ->
    ok.

