-module(woomsg_md5).
-export([md5_binary/1,
         md5_hex/1]).

%% External APIs:

%% 返回Source的二进制的MD5值
%% Source:iolist() | binary()
%%
%% 返回: Res:binary()
md5_binary(Source) ->
    erlang:md5(Source).

%% 返回Source的16进制的MD5值
%% Source:iolist() | binary()
%%
%% 返回: Res:list()
md5_hex(Source) ->
    Md5_bin = erlang:md5(Source),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

%% Internal APIs:

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).


