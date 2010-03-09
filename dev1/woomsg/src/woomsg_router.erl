-module(woomsg_router).
-include("woomsg_configure.hrl").
-export([handle_request/1]).


handle_request(Req) ->
    "/" ++ Path = Req:get(path),
    %% 解析出Path的前缀:
    %% E.g. 
    %% login/user -> login
    %% login      -> login
    PrefixPath = woomsg_util:list_index_prefix($/, Path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case PrefixPath of
		"" ->
		    index_controller:handle_get(Req);
		"index" ->
		    index_controller:handle_get(Req);
                "api" ->
		    api_controller:handle_get(Req);
                "blog" ->
		    blog_controller:handle_get(Req);
		"contact" ->
		    contact_controller:handle_get(Req);
		"faq" ->
		    faq_controller:handle_get(Req);
                "image" ->
		    image_controller:handle_get(Req);
		"login" ->
		    login_controller:handle_get(Req);
                "logout" ->
		    logout_controller:handle_get(Req);
		"nopage" ->
		    nopage_controller:handle_get(Req);
		"publictimeline" ->
		    publictimeline_controller:handle_get(Req);
		"register" ->
		    register_controller:handle_get(Req);
		"search" ->
		    search_controller:handle_get(Req);
		"service" ->
		    service_controller:handle_get(Req);
		"setting" ->
		    setting_controller:handle_get(Req);
		"showpic" ->
		    showpic_controller:handle_get(Req);
		"showpicfull" ->
		    showpicfull_controller:handle_get(Req);
		"support" ->
		    support_controller:handle_get(Req);
		"tag" ->
		    tag_controller:handle_get(Req);
		"upload" ->
		    upload_controller:handle_get(Req);
		"user" ->
		    user_controller:handle_get(Req);
                _ ->
                    Req:serve_file(Path, ?NFS_PREFIX)
            end;
        'POST' ->
            case PrefixPath of
                "comment" ->
		    comment_controller:handle_post(Req);
                "following" ->
		    following_controller:handle_post(Req);
		"login" ->
		    login_controller:handle_post(Req);
                "logout" ->
		    logout_controller:handle_post(Req);
                "pic" ->
		    pic_controller:handle_post(Req);
                "register" ->
		    register_controller:handle_post(Req);
		"tag" ->
		    tag_controller:handle_post(Req);
                "upload" ->
		    upload_controller:handle_post(Req);
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.
