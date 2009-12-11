%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(woomsg).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the woomsg server.
start() ->
    woomsg_deps:ensure(),
    ensure_started(crypto),
    application:start(woomsg).

%% @spec stop() -> ok
%% @doc Stop the woomsg server.
stop() ->
    Res = application:stop(woomsg),
    application:stop(crypto),
    Res.
