%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(stickynotes).
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
%% @doc Start the stickynotes server.
start() ->
    stickynotes_deps:ensure(),
    ensure_started(crypto),
    application:start(stickynotes).

%% @spec stop() -> ok
%% @doc Stop the stickynotes server.
stop() ->
    Res = application:stop(stickynotes),
    application:stop(crypto),
    Res.
