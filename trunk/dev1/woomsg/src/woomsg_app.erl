%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the woomsg application.

-module(woomsg_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for woomsg.
start(_Type, _StartArgs) ->
    %% add init code here:
    %% - start - 

    %% - end - 

    woomsg_deps:ensure(),
    woomsg_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for woomsg.
stop(_State) ->
    %% add cleanup code here:
    %% - start - 

    %% - end - 
    ok.
