-module(contact_controller).
-export([handle_get/1, handle_post/1]).

handle_get(Req) ->
    Data = contact_view:index("test data"),
    Req:respond({200, [{"Content-Type","text/html"}], Data}).

handle_post(_Req) ->
    ok.
