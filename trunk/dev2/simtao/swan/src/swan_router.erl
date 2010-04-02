-module(swan_router).
-export([handle_request/1]).

handle_request(Req) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->  
	    case Path of
		"api/text" ->
		    swan_text:handle_get(Req);
		"api/keywords" ->
		    swan_keywords:handle_get(Req);
		"service/analyze" ->
		    swan_analyze:handle_get(Req);
		"test/utf8" ->
		    swan_test_utf8:handle_get(Req);
		"test/gb2312" ->
		    swan_test_gb2312:handle_get(Req);
		_ ->
	            Req:respond({200, [{"Content-Type", "text/plain"}], "bad get request"})
	    end;
        'POST' ->
	    case Path of
		"service/analyze" ->
		    swan_analyze:handle_post(Req);
		_ ->
		    Req:respond({200, [{"Content-Type", "text/plain"}], "bad post request"})
	    end;
        _ ->
            Req:respond({501, [], []})
    end.
