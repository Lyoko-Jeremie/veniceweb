%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(swan).
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
%% @doc Start the swan server.
start() ->
    swan_deps:ensure(),
    ensure_started(crypto),
    application:start(swan).

%% @spec stop() -> ok
%% @doc Stop the swan server.
stop() ->
    Res = application:stop(swan),
    application:stop(crypto),
    Res.
