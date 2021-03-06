%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the swan application.

-module(swan_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for swan.
start(_Type, _StartArgs) ->
    swan_deps:ensure(),

    %% start your applications here:
    inets:start(),
    iconv:start(),

    swan_dict_server:start(),

    swan_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for swan.
stop(_State) ->

    %% stop your applications here:
    swan_dict_server:stop(),

    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
