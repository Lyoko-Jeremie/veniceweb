-module(upload_controller).
-export([handle_get/1, handle_post/1]).

-define(PIC_MESSAGE_KEY, "message").

handle_get(Req) ->
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            Data = upload_view:index(login, Username, undefined),
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


handle_post(Req) ->
    case woomsg_common:user_state(Req) of
        {login, Username} ->
            UploadRes = handle_upload_process(Req, Username),
            Data = upload_view:index(login, Username, UploadRes),
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


handle_upload_process(Req, Username) ->
    case woomsg_upload:parse_form_pic(Req, false) of
	%% Bug fix:
	%% 有时候客户端会附带传递x和y的坐标过来, 需要识别
        [{Path, Guid, Type}, {text, ?PIC_MESSAGE_KEY, Message}, {text, "x", _}, {text, "y", _}] ->
            case woomsg_image:convert_pic(Path, Guid, Type) of
                true ->
                    %% 文件上传成功:
                    %% 
                    case woomsg_pic:new_pic(Guid, Username, Path, Type, Message) of
                        ok ->
                            io:format("new pic success~n", []),
                            upload_ok;
                        error ->
                            io:format("new pic error~n", []),
                            upload_error_new_pic
                    end;
                false ->
                    io:format("Convert pic error!~n", []),
                    upload_error_convert
            end;
	[{Path, Guid, Type}, {text, ?PIC_MESSAGE_KEY, Message}] ->
            case woomsg_image:convert_pic(Path, Guid, Type) of
                true ->
                    %% 文件上传成功:
                    %% 
                    case woomsg_pic:new_pic(Guid, Username, Path, Type, Message) of
                        ok ->
                            io:format("new pic success~n", []),
                            upload_ok;
                        error ->
                            io:format("new pic error~n", []),
                            upload_error_new_pic
                    end;
                false ->
                    io:format("Convert pic error!~n", []),
                    upload_error_convert
            end;
        _->
            %% TODO: 错误处理
            io:format("Upload pic error!~n", []),
            upload_error
    end.



