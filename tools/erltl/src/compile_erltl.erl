-module(compile_erltl).
-export([make/3]).

%% 功能:
%%   遍历ErlTLPath下所有*.erltl结尾的文件, 对其进行编译, 
%%   编译之后的beam文件存放在EBinPath;
%%   每个erltl对应的源文件存放在OutSrcPath.
%%
%%
%% 参数:
%%   ErlTLPath:string()  存放erltl模板文件的路径
%%   EBinPath:string()   保存编译出来的*.beam文件的路径
%%   OutSrcPath:string() 保存编译模板生成的源文件*.erl的路径
%%                       (这些文件只是方便观察*.erltl经过编译后的*.erl文件的结构, 
%%                        不影响实际运行)
%%
%% 返回值:
%%  ok
make(ErlTLPath, EBinPath, OutSrcPath) ->
    %% 创建EBinPath, OutSrcPath, 如果这两个目录已经存在, 忽略{error, eexit}
    file:make_dir(EBinPath),
    file:make_dir(OutSrcPath),

    filelib:fold_files(ErlTLPath,
                       ".+\.erltl$",
		       true,  %% recursive
		       fun(F, _Acc) ->
			       ErlTLBaseName = filename:basename(filename:rootname(F)),
                               io:format("Compiling ErlTL file ~p~n",[ErlTLBaseName]),
			       erltl:compile(F,
					     [{outdir, EBinPath},
                                              {outesrc, OutSrcPath},
                                              nowarn_unused_vars,
					      show_errors,
					      show_warnings])
                       end,
		       []),
    io:format("the *.erltl files is under: ~p~n", [ErlTLPath]),
    io:format("the *.beam files is under: ~p~n", [EBinPath]),
    io:format("the *.erl files is under: ~p~n", [OutSrcPath]),
    io:format("compile ErlTL files complete!~n", []),
    ok.
