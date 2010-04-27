-module(notes_controller,[Env]).

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    Json = erails_var:get_param("json", Env),
    Notes = mochijson2:decode(Json),
    error_logger:info_msg("~p~n", [Notes]),
    {render,"notes/index.html",[{data, "notes"}]}.

before_filter() ->
    FilterOnly = ["index"],
    case lists:member(erails_var:get_action(Env), FilterOnly) of
	true ->
	    ok;
	false ->
	    {text, "no action!!!"}
    end.
