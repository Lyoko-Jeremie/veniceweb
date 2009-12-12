-module(image_util).
-export([convert/5]).

%%
%% @Focus: true 不保持原图的比例; false 保持原图的比例
%%
convert(SourceFilename, TargetFilename, Width, Height, Focus) ->
    Cmd = case Focus of
	      true ->
		  io_lib:format("convert -resize ~px~p! ~s ~s", [Width, Height, SourceFilename, TargetFilename]);
	      false ->
		  io_lib:format("convert -resize ~px~p ~s ~s", [Width, Height, SourceFilename, TargetFilename])
	  end,
    case os:cmd(lists:flatten(Cmd)) of
	[] ->
	    ok;
	_ ->
	    error
    end.
