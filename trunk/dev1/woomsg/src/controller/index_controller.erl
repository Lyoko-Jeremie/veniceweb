-module(index_controller).
-export([handle_get/2, handle_post/2]).

handle_get(Req, _DocRoot) ->
    Data = index_view:index("test data"),
    Req:respond({200, [{"Content-Type","text/html"}], Data}).

handle_post(_Req, _DocRoot) ->
    ok.
