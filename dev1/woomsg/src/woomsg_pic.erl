-module(woomsg_pic).
-export([new_pic/5,
         get_pic/1,
         delete_pic/1,
         inc_count/1, inc_count/2, dec_count/1, dec_count/2,
         inc_dig/1, inc_dig/2, dec_dig/1, dec_dig/2,
         set_spam/2]).

%% 使用建议:
%% <1>不建议单独使用inc_count/1, inc_count/2, dec_count/1, dec_count/2
%%    来修改评论,图片的评论数会在修改pic_comment的事务中自动更新.
%%

%% 根据图片的GUID返回一张图片
%% 成功返回: {}
%% 失败返回: []
get_pic(Guid) ->
    F = fun() ->
	    mnesia:read({pic, Guid})
	end,
    case mnesia:transaction(F) of
	{atomic, [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}]} ->
	    {Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate};
	_ ->
	    []
    end.

%% 增加一张照片
%% 成功返回: ok
%% 失败返回: error
new_pic(Guid, Owner, Path, Type, Msg) ->
    CreateDate = woomsg_datetime:get_datetime(),
    F = fun() ->
	    mnesia:write({pic, Guid, Owner, Path, Type, Msg, 0, 0, [], 0, CreateDate})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% TODO: 这个函数存在Bug, 需要修正
%%        
%% 删除一张照片:
%% (会自动把照片相关的评论一起删除)
%% 成功返回: ok
%% 失败返回: error
%%
%% 补充:
%% 我们使用mnesia:index_match_object(Pattern, Pos)
%% 来代替mnesia:match_object(Pattern)
%% 理由是: 使用前者可以利用索引的信息来加速匹配
delete_pic(Guid) ->
    F = fun() ->
	    case mnesia:delete({pic, Guid}) of
                ok ->
		    %% 更新pic_comment表, 删除所有的评论信息
                    io:format("delete success~n", []),
                    case mnesia:index_match_object(pic_comment, {pic_comment, '_', Guid, '_', '_', '_'}, pic_guid) of 
			[] ->
			    ok;
			ValList when is_list(ValList) ->
			    io:format("ValList: ~p~n", ValList),
			    lists:foreach(fun({pic_comment, CommentGuid, _Guid, _, _, _} = _Elem) ->
					      mnesia:delete({pic_comment, CommentGuid})
					  end, ValList),
			    ok;
			Any ->
                            io:format("Any:~p~n", [Any]),
			    error
                    end;
		_ ->
                    io:format("delete error~n", []),
		    error
            end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

inc_count(Guid) ->
    inc_count(Guid, 1).

%% 增加评论的数量
%% 成功返回: ok
%% 失败返回: error
inc_count(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count + Num, Dig, TagList, Spam, CreateDate});
	        _ ->
		    error
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

dec_count(Guid) ->
    dec_count(Guid, 1).

%% 减少评论的数量:
%% (如果要减少的数量>当前的评论书, 则评论重置为0)
%% 成功返回: ok
%% 失败返回: error
dec_count(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    case Count > Num of
		        true ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count - Num, Dig, TagList, Spam, CreateDate});
			false ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, 0, Dig, TagList, Spam, CreateDate})
                    end; 
	        _ ->
		    error
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
    
inc_dig(Guid) ->
    inc_dig(Guid, 1).

%% 增加dig的数量
%% 成功返回: ok
%% 失败返回: error
inc_dig(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig + Num, TagList, Spam, CreateDate});
	        _ ->
		    error
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

dec_dig(Guid) ->
    dec_dig(Guid, 1).

%% 减少dig的数量:
%% (如果要减少的数量>当前的dig数, 则dig重置为0)
%% 成功返回: ok
%% 失败返回: error
dec_dig(Guid, Num) ->
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}] ->
		    case Dig > Num of
		        true ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig - Num, TagList, Spam, CreateDate});
			false ->
                            mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, 0, TagList, Spam, CreateDate})
                    end; 
	        _ ->
		    error
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%% 设置图片是否为垃圾信息
%% true  - 垃圾信息
%% false - 正常图片
%% 成功返回: ok
%% 失败返回: error
set_spam(Guid, ArgSpam) ->
    ResSpam = case ArgSpam =:= true of
                  true ->
		      1;
	          _ ->
		      0
              end,
    F = fun() ->
	    case mnesia:read({pic, Guid}) of
	        [{pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, _Spam, CreateDate}] ->
                     mnesia:write({pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, ResSpam, CreateDate});
	        _ ->
		    error
            end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
