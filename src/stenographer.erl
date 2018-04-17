-module(stenographer).
-export([send/2, send/3]).

send(Measurement, Fields) ->
    send(stenographer_line:encode(Measurement,
                                  Fields,
                                  stenographer_utils:add_default_tags([]),
                                  stenographer_utils:timestamp())).

send(Measurement, Fields, Tags) ->
    send(stenographer_line:encode(Measurement,
                                  Fields,
                                  stenographer_utils:add_default_tags(Tags),
                                  stenographer_utils:timestamp())).

% private

send(Data) ->
    gen_server:cast(stenographer, {send, Data}).
