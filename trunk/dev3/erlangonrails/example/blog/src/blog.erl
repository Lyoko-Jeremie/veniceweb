%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(blog).
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
%% @doc Start the blog server.
start() ->
    blog_deps:ensure(),
    ensure_started(crypto),
    %% start our memory db :)
    blogdb:start(),
    application:start(blog).

%% @spec stop() -> ok
%% @doc Stop the blog server.
stop() ->
    Res = application:stop(blog),
    application:stop(crypto),
    Res.
