-module(stenographer_tests).
-define(MOD, stenographer).
-include_lib("eunit/include/eunit.hrl").

default_tags_test() ->
    register(stenographer, self()),
    application:set_env(stenographer, tags, [{a, 1}]),
    application:set_env(stenographer, host_as_tag, false),
    application:set_env(stenographer, node_name_as_tag, false),
    stenographer:send(a, [{b, 1}]),
    ?assertMatch([{send,<<"a,a=1 b=1 ", _/binary>>}], flush_cast()),
    unregister(stenographer).

host_as_tag_test() ->
    register(stenographer, self()),
    application:set_env(stenographer, tags, []),
    application:set_env(stenographer, host_as_tag, true),
    application:set_env(stenographer, node_name_as_tag, false),
    stenographer:send(a, [{b, 1}]),
    ?assertMatch([{send,<<"a,host=", _/binary>>}], flush_cast()),
    unregister(stenographer).

node_name_as_tag_test() ->
    register(stenographer, self()),
    application:set_env(stenographer, tags, []),
    application:set_env(stenographer, host_as_tag, false),
    application:set_env(stenographer, node_name_as_tag, true),
    stenographer:send(a, [{b, 1}]),
    ?assertMatch([{send,<<"a,node=nonode@nohost b=1 ", _/binary>>}], flush_cast()),
    unregister(stenographer).

flush_cast() ->
    flush_cast([]).

flush_cast(Acc) ->
    receive
        {'$gen_cast', X} -> flush_cast([X|Acc])
    after
        0 -> Acc
    end.
