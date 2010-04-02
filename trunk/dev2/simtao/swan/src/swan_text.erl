-module(swan_text).
-export([handle_get/1]).

handle_get(Req) ->
    case Req:parse_qs() of
	[{"code", _Code}] ->
	    {_Host, Referer} = swan_util:parse_host_and_referer(Req),
	    Text = swan_http:process_url(Referer),
            Req:respond({200, [{"Content-Type", "text/plain"}], Text});
	_ ->
	    Data = "api-request-error;",
            Req:respond({200, [{"Content-Type", "text/plain"}], Data})	    
    end.
