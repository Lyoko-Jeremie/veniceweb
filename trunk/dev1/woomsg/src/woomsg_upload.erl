-module(woomsg_upload).
-include("woomsg_configure.hrl").
-export([parse_form_pic/1, parse_form_pic/2, 
         parse_form_photo/1, parse_form_photo/2,
         parse_form/3]).

-record(state, {filename, file, path, guid, type, key, value}).

parse_form_photo(Req) ->
    parse_form_photo(Req, false).

parse_form_pic(Req) ->
    parse_form_pic(Req, false).

parse_form_photo(Req, Debug) ->
    parse_form(Req, Debug, fun(State)-> update_state_photo(State) end).

parse_form_pic(Req, Debug) ->
    parse_form(Req, Debug, fun(State) -> update_state_pic(State) end).

parse_form(Req, Debug, UpdateStateCallback) ->
    Callback = fun(Next) -> callback(Next, #state{}, [], Debug, UpdateStateCallback) end,
    {_, _, Res} = mochiweb_multipart:parse_multipart_request(Req, Callback),
    Res.

%% Headers: 可能的形式(其中C是input-file为空的形式)
%% 一次提交一条text和一个file, 也是分开一条条发送的.
%%
%% message - textarea
%% media   - file
%%
%% a. 一个text
%% b. 一空的text (和a的Header一样)
%% c. 一个file
%% d. 一个空file
%%
%% a. Headers: [{"content-disposition",{"form-data",[{"name","message"}]}}]        
%% b. Headers: [{"content-disposition",{"form-data",[{"name","message"}]}}]        
%% c. Headers: [{"content-disposition",{"form-data",[{"name","media"},
%%                                                   {"filename","Sunset.jpg"}]}},
%%              {"content-type",{"image/jpeg",[]}}]
%% d. Headers: [{"content-disposition",{"form-data",[{"name","media"},{"filename",[]}]}},
%%              {"content-type",{"application/octet-stream",[]}}]
%% 文件格式:
%% jpeg -  {"content-type",{"image/jpeg",[]}}
%% png  -  {"content-type",{"image/png",[]}}
%% gif  -  {"content-type",{"image/gif",[]}}
%% bmp  -  {"content-type",{"image/bmp",[]}}
callback(Next, State, Acc, Debug, UpdateStateCallback) ->
    case Next of
        {headers, Headers} ->
            {"form-data", H1} = proplists:get_value("content-disposition", Headers),
	    Name = proplists:get_value("name", H1),
	    Filename = proplists:get_value("filename", H1),
	    NewState = case Filename of
		           undefined ->
			       #state{key=Name};
			   [] ->
			       State;
			    _ ->
			       %% 这个文件名会在UpdateStateCallback中修改.
			       #state{filename = Filename, type=verify_type_in_header(Headers)}
                       end,
	    case Debug of
	        true ->
		    io:format("Headers: ~p~n", [Headers]),
		    io:format("NewState: ~p~n", [NewState]),
                    io:format("NewAcc: ~p~n", [Acc]),
	            fun(N) -> callback(N, NewState, Acc, Debug, UpdateStateCallback) end;
		false ->
	            fun(N) -> callback(N, NewState, Acc, Debug, UpdateStateCallback) end
	    end;
	{body, Body} ->
	    if
	        State#state.filename =/= undefined ->
	            if
		        State#state.file =/= undefined ->
			    file:write(State#state.file, Body),
			    NewState = State,
			    NewAcc   = Acc;
			true ->
			    if
			        State#state.type =:= unknown ->
				    %% 未知的文件类型, 跳过
				    %% TODO: 合理的记录错误日志
				    io:format("Upload Warning: unknown file type ~p~n", [State#state.filename]),
			            NewState = State,
				    NewAcc   = Acc;
				true ->
                                    TmpState = UpdateStateCallback(State),
			            case file:open(TmpState#state.filename, [raw, write]) of
				        {ok, File} ->
			                    file:write(File, Body),
				            NewState = TmpState#state{file = File},
				            NewAcc   = Acc;
				        {error, Error} ->
				            %% TODO: 合理的记录错误日志
				            io:format("Upload Error: could not open ~p for writing, error#~p~n", [State#state.filename, Error]),
				            NewState = State,
				            NewAcc   = Acc,
			            exit(could_not_open_file_for_writeing)
			        end
			    end
		    end;
		true ->
		    if
		        State#state.key =/= undefined ->
		            NewState = #state{},
			    NewAcc   = [{text, State#state.key, Body} | Acc];
			true ->
			    NewState = State,
			    NewAcc   = Acc
		    end
            end,
	    case Debug of
	        true ->
		    io:format("Body: ~p~n", ["---"]),
		    io:format("NewState: ~p~n", [NewState]),
		    io:format("NewAcc: ~p~n", [NewAcc]),
                    fun(N) -> callback(N, NewState, NewAcc, Debug, UpdateStateCallback) end;
		false ->
                    fun(N) -> callback(N, NewState, NewAcc, Debug, UpdateStateCallback) end
            end;
	body_end ->
	    if
	        State#state.file =/= undefined ->
                    file:close(State#state.file),
		    NewAcc = [{State#state.path, State#state.guid, State#state.type} | Acc];
		true ->
		    NewAcc = Acc
            end,
	    case Debug of
	        true ->
		    io:format("Body_End: ~n", []),
		    io:format("NewState: ~p~n", [#state{}]),
		    io:format("NewAcc: ~p~n", [NewAcc]),
	            fun(N) -> callback(N, #state{}, NewAcc, Debug, UpdateStateCallback) end;
		false ->
	            fun(N) -> callback(N, #state{}, NewAcc, Debug, UpdateStateCallback) end
	    end;
	eof ->
	    lists:reverse(Acc);
	_ ->
	    fun(N) -> callback(N, State, Acc, Debug, UpdateStateCallback) end
    end.

%% Internal APIs:

%% 绝对路径: ?NFS_PREFIX ++ State#path ++ "/ori/" State#guid ++ State#type
update_state_photo(State) ->
    Path = woomsg_nfs_cache:get_photo_path(),
    Guid = woomsg_guid:get_image_guid(),
    NewState = State#state{path = Path,
                           guid = Guid,
		 	   filename = ?NFS_PREFIX ++ Path ++ "/ori/" ++ Guid ++ State#state.type},
    NewState.

%% 绝对路径: ?NFS_PREFIX ++ State#path ++ "/ori/" State#guid ++ State#type
update_state_pic(State) ->
    Path = woomsg_nfs_cache:get_pic_path(),
    Guid = woomsg_guid:get_image_guid(),
    NewState = State#state{path = Path,
                           guid = Guid,
		 	   filename = ?NFS_PREFIX ++ Path ++ "/ori/" ++ Guid ++ State#state.type},
    NewState.

%%
%% 解析Headers中的文件类型
%% jpeg
%% png
%% gif
%% bmp
%% unknown
verify_type_in_header(Headers) ->
    case proplists:get_value("content-type", Headers) of
        {"image/jpeg", _} ->
            ".jpg";
	{"image/png", _} ->
	    ".png";
	{"image/gif", _} ->
	    ".gif";
	{"image/bmp", _} ->
	    ".bmp";
	_ ->
	    unknown
    end.
