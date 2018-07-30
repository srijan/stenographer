-module(stenographer_utils).

-export([add_default_tags/1, timestamp/0]).

add_default_tags(Tags) ->
    merge_tags(host_tag()++node_name_tag(), merge_tags(default_tags(), Tags)).

% private

default_tags() ->
    {ok, Tags} = application:get_env(stenographer, tags),
    Tags.

host_tag() ->
    case application:get_env(stenographer, host_as_tag) of
        {ok, true} ->
            [{host, net_adm:localhost()}];
        _ ->
            []
    end.

node_name_tag() ->
    case application:get_env(stenographer, node_name_as_tag) of
        {ok, true} ->
            [{node, node()}];
        _ ->
            []
    end.

merge_tags(A, B) when is_list(A) ->
    merge_tags(maps:from_list(A), B);
merge_tags(A, B) when is_list(B) ->
    merge_tags(A, maps:from_list(B));
merge_tags(A, B) ->
    maps:merge(A, B).

timestamp() ->
    os:system_time(nano_seconds).
