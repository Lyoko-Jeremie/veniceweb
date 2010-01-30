-module(woomsg_pic_hook).
-export([process_pic/2,
         process_pic_limit/3, process_pic_limit/4,
	 process_pic_with_user_photo/2,
	 process_pic_limit_with_user_photo/3, process_pic_limit_with_user_photo/4]).

%% 处理PicList的结果
%% 原始的Pic
%% {pic, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}
%%
%% <1> Pic:
%% {pic, Tag, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}
%% <2> Pic (with user photo) 增加了用户的头像信息.
%% {pic, Tag, Guid, Owner, PhotoGuid, PhotoPath, PhotoType, Path, Type, Msg, Count, Dig, TagList, Spam, CreateDate}

%%
%% 返回按照时间CreateDate排序的Pic
%% Pic:
%% <1> 返回的Pic中增加了一个标签: true | false
%%      true 表示我们就是这张照片的拥有者, 可以删除
%%      false 表示不能删除这条评论
%% <2> 格式化了CreateDate
%%
process_pic({0, []}, _Username) ->
    {0, []};
process_pic({Count, PicList}, Username) ->
    %% 按照CreateDate来排序
    SortPicList = lists:sort(fun({pic, _Guid1, _Owner1, _Path1, _Type1, _Msg1, _Count1, _Dig1, _TagList1, _Spam1, CreateDate1},
				 {pic, _Guid2, _Owner2, _Path2, _Type2, _Msg2, _Count2, _Dig2, _TagList2, _Spam2, CreateDate2}) ->
			         case woomsg_datetime:compare_datetime(CreateDate1, CreateDate2) of
				     big ->
				         true;
				     equal ->
					 true;
				     small ->
				         false
 				 end
    			     end, PicList),

    %% 格式化Pic, 增加用户信息.
    Res = lists:foldl(fun({pic, Guid, Owner, Path, Type, Msg, _Count, Dig, TagList, Spam, CreateDate}, AccIn) ->
		          Tag = (Owner =:= Username),
			  [{pic, Tag, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, woomsg_datetime:get_fmt_since_datetime_string(CreateDate)} | AccIn]
		      end, [], SortPicList),
    {Count, lists:reverse(Res)}.

process_pic_limit(PicList, Username, Len) ->
    process_pic_limit(PicList, Username, 1, Len).

%% 提供了类似Limit的功能:
%% (Start从1开始)
%% 返回结果:
%% {0, []}
%% {out_of_index, []}
%% {Count, Pic:list()}
process_pic_limit({0, []}, _Username, _Start, _Len) ->
    {0, []};
process_pic_limit({Count, PicList}, Username, Start, Len) ->
    %% 按照CreateDate来排序
    SortPicList = lists:sort(fun({pic, _Guid1, _Owner1, _Path1, _Type1, _Msg1, _Count1, _Dig1, _TagList1, _Spam1, CreateDate1},
				 {pic, _Guid2, _Owner2, _Path2, _Type2, _Msg2, _Count2, _Dig2, _TagList2, _Spam2, CreateDate2}) ->
			         case woomsg_datetime:compare_datetime(CreateDate1, CreateDate2) of
				     big ->
				         true;
				     equal ->
					 true;
				     small ->
				         false
 				 end
    			     end, PicList),

    case erlang:length(SortPicList) < Start of
	true ->
	    {out_of_index, []};
	false ->
	    ResList = lists:sublist(SortPicList, Start, Len),
            %% 格式化Pic, 增加用户信息.
            Res = lists:foldl(fun({pic, Guid, Owner, Path, Type, Msg, _Count, Dig, TagList, Spam, CreateDate}, AccIn) ->
		                  Tag = (Owner =:= Username),
			          [{pic, Tag, Guid, Owner, Path, Type, Msg, Count, Dig, TagList, Spam, woomsg_datetime:get_fmt_since_datetime_string(CreateDate)} | AccIn]
		              end, [], ResList),
            {erlang:length(Res), lists:reverse(Res)}
    end.


%% 返回按照时间CreateDate排序的Pic
%% Pic:
%% <1> 返回的Pic中增加了一个标签: true | false
%%      true 表示我们就是这张照片的拥有者, 可以删除
%%      false 表示不能删除这条评论
%% <2> 增加了这条评论的用户信息(PhotoGuid, PhotoPath, PhotoType)
%% <3> 格式化了CreateDate
%%
process_pic_with_user_photo({0, []}, _Username) ->
    {0, []};
process_pic_with_user_photo({Count, PicList}, Username) ->
    %% 按照CreateDate来排序
    SortPicList = lists:sort(fun({pic, _Guid1, _Owner1, _Path1, _Type1, _Msg1, _Count1, _Dig1, _TagList1, _Spam1, CreateDate1},
				 {pic, _Guid2, _Owner2, _Path2, _Type2, _Msg2, _Count2, _Dig2, _TagList2, _Spam2, CreateDate2}) ->
			         case woomsg_datetime:compare_datetime(CreateDate1, CreateDate2) of
				     big ->
				         true;
				     equal ->
					 true;
				     small ->
				         false
 				 end
    			     end, PicList),

    %% 格式化Pic, 增加用户信息.
    Res = lists:foldl(fun({pic, Guid, Owner, Path, Type, Msg, _Count, Dig, TagList, Spam, CreateDate}, AccIn) ->
		          Tag = (Owner =:= Username),
			  case woomsg_user:get_user(Owner) of
			      {Owner, _Pwd, _Email, PhotoGuid, PhotoPath, PhotoType} ->
			          [{pic, Tag, Guid, Owner, PhotoGuid, PhotoPath, PhotoType, Path, Type, Msg, Count, Dig, TagList, Spam, woomsg_datetime:get_fmt_since_datetime_string(CreateDate)} | AccIn];
			      [] ->
				  AccIn
			  end
		      end, [], SortPicList),
    {Count, lists:reverse(Res)}.


process_pic_limit_with_user_photo(PicList, Username, Len) ->
    process_pic_limit_with_user_photo(PicList, Username, 1, Len).

%% 提供了类似Limit的功能:
%% (Start从1开始)
%% 返回结果:
%% {0, []}
%% {out_of_index, []}
%% {Count, Pic:list()}
process_pic_limit_with_user_photo({0, []}, _Username, _Start, _Len) ->
    {0, []};
process_pic_limit_with_user_photo({Count, PicList}, Username, Start, Len) ->
    %% 按照CreateDate来排序
    SortPicList = lists:sort(fun({pic, _Guid1, _Owner1, _Path1, _Type1, _Msg1, _Count1, _Dig1, _TagList1, _Spam1, CreateDate1},
				 {pic, _Guid2, _Owner2, _Path2, _Type2, _Msg2, _Count2, _Dig2, _TagList2, _Spam2, CreateDate2}) ->
			         case woomsg_datetime:compare_datetime(CreateDate1, CreateDate2) of
				     big ->
				         true;
				     equal ->
					 true;
				     small ->
				         false
 				 end
    			     end, PicList),

    case erlang:length(SortPicList) < Start of
	true ->
	    {out_of_index, []};
	false ->
	    ResList = lists:sublist(SortPicList, Start, Len),
            %% 格式化Pic, 增加用户信息.
            Res = lists:foldl(fun({pic, Guid, Owner, Path, Type, Msg, _Count, Dig, TagList, Spam, CreateDate}, AccIn) ->
		                  Tag = (Owner =:= Username),
				  case woomsg_user:get_user(Owner) of
			              {Owner, _Pwd, _Email, PhotoGuid, PhotoPath, PhotoType} ->
			                  [{pic, Tag, Guid, Owner, PhotoGuid, PhotoPath, PhotoType, Path, Type, Msg, Count, Dig, TagList, Spam, woomsg_datetime:get_fmt_since_datetime_string(CreateDate)} | AccIn];
			              [] ->
				          AccIn
			          end
		              end, [], ResList),
            {erlang:length(Res), lists:reverse(Res)}
    end.


