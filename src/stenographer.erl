-module(stenographer).
-export([send/2, send/3]).

send(Measurement, Fields) ->
	send(stenographer_line:encode_simple(Measurement, Fields) ).

send(Measurement, Fields, Tags) ->
	send(stenographer_line:encode(Measurement, Fields, Tags)).

% private

send(Data) ->
    gen_server:cast(stenographer, {send, Data}).
