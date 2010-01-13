-module(woomsg_comment_hook).
-export([process_comment/2]).

%% 处理CommentList
%% Comment: {Guid, PicGuid, Owner, Comment, CreateDate}
%%
%% 返回按照时间CreateDate排序的Comments
%% Comment: {BSelf, Guid, PicGuid, Owner, PhotoGuid, PhotoPath, PhotoType, Comment, FormatCreateDate}
%% <1> 我们在返回的Comment中增加了一个标签: true | false
%%     true表示我们就是这条评论的拥有者, 可以删除.
%%     false表示不能删除这条评论
%% <2> 增加了这条评论的用户信息(PhotoGuid, PhotoPath, PhotoType)
%% <3> 格式化了CreateDate
%%
process_comment({0, []}, _Username) ->
    {0, []};
process_comment({Count, CommentList}, Username) ->
    %% 按照CreateDate来排序
    SortCommentList = lists:sort(fun({_Guid1, _PicGuid2, _Owner1, _Comment1, CreateDate1},
                                     {_Guid2, _PicGuid2, _Owner2, _Comment2, CreateDate2}) ->
				     case woomsg_datetime:compare_datetime(CreateDate1, CreateDate2) of
				         big ->
					     true;
					 equal ->
					     true;
					 small ->
					     false	
                                     end
			         end, CommentList),
    %% 格式化Comment, 增加用户信息
    Res = lists:foldl(fun({Guid, PicGuid, Owner, Comment, CreateDate}, AccIn) ->
		          Tag = (Owner =:= Username),
			  case woomsg_user:get_user(Owner) of
			      {Owner, _Pwd, _Email, PhotoGuid, PhotoPath, PhotoType} ->
			          [{Tag, Guid, PicGuid, Owner, PhotoGuid, PhotoPath, PhotoType, Comment, woomsg_datetime:get_fmt_since_datetime_string(CreateDate)} | AccIn];
			      [] ->
				  AccIn
			  end
	              end, [], SortCommentList),
    {Count, lists:reverse(Res)}.
