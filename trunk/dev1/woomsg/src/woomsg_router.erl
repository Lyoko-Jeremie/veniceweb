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
                "api" ->
		    api_controller:handle_get(Req, DocRoot);
		"faq" ->
		    faq_controller:handle_get(Req, DocRoot);
		"publictimeline" ->
		    publictimeline_controller:handle_get(Req, DocRoot);
		"setting" ->
		    setting_controller:handle_get(Req, DocRoot);
		"support" ->
		    support_controller:handle_get(Req, DocRoot);
		"tag" ->
		    tag_controller:handle_get(Req, DocRoot);
		"upload" ->
		    upload_controller:handle_get(Req, DocRoot);
		"user" ->
		    user_controller:handle_get(Req, DocRoot);
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
