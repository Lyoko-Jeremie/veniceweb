-module(upload_controller).
-export([handle_get/2, handle_post/2]).

-define(PIC_MESSAGE_KEY, "message").

handle_get(Req, _DocRoot) ->
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            Data = upload_view:index(login, Username),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
        {logout_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});
        {logout_remember, _Username} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});            
        {logout_no_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []})            
    end.


handle_post(Req, _DocRoot) ->
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            case woomsg_upload:parse_form_pic(Req, false) of
	        [{Path, Guid, Type}, {text, ?PIC_MESSAGE_KEY, Message}] ->
	            case woomsg_image:convert_pic(Path, Guid, Type) of
		        true ->
			    %% 文件上传成功:
			    %% 
			    case woomsg_pic:new_pic(Guid, Username, Path, Type, Message) of
			        ok ->
			            io:format("new pic success~n", []);
			        error ->
			            io:format("new pic error~n", [])
			    end;
			false ->
			    io:format("Convert pic error!~n", [])
                    end;
		_ ->
		    %% TODO: 错误处理
	            io:format("Upload pic error!~n", [])
            end,
            Data = upload_view:index(login, Username),
            Req:respond({200, [{"Content-Type", "text/html"}], Data});
        {logout_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});
        {logout_remember, _Username} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []});            
        {logout_no_remember, undefined} ->
            %% 用户没有登录, 重定向到主页
            Req:respond({302, [{"Location", "/"}], []})            
    end.



