-module(woomsg_router).
-export([handle_request/2]).


handle_request(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
		"" ->
		    index_controller:handle_get(Req, DocRoot);
		"index" ->
		    index_controller:handle_get(Req, DocRoot);
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.
