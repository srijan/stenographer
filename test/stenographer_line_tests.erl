-module(stenographer_line_tests).
-define(MOD, stenographer_line).
-include_lib("eunit/include/eunit.hrl").

to_key_test() ->
	?assertEqual(<<"test">>, ?MOD:to_key(test)),
	?assertEqual(<<"test">>, ?MOD:to_key("test")),
	?assertEqual(<<"test">>, ?MOD:to_key(<<"test">>)),
	?assertEqual(<<"123">>, ?MOD:to_key(123)).

to_val_test() ->
	?assertEqual(<<"\"test\"">>, ?MOD:to_val(test)),
	?assertEqual(<<"\"test\"">>, ?MOD:to_val("test")),
	?assertEqual(<<"\"test\"">>, ?MOD:to_val(<<"test">>)),
	?assertEqual(<<"true">>, ?MOD:to_val(true)),
	?assertEqual(<<"false">>, ?MOD:to_val(false)),
	?assertEqual(<<"\"te\\ st\"">>, ?MOD:to_val(<<"te st">>)),
	?assertEqual(<<"\"te\\,st\"">>, ?MOD:to_val(<<"te,st">>)),
	?assertEqual(<<"\"te\\\"st\"">>, ?MOD:to_val(<<"te\"st">>)),
	?assertEqual(<<"\"t\\ e\\\"s\\,t\\\"\"">>, ?MOD:to_val(<<"t e\"s,t\"">>)),
	?assertEqual(<<"123">>, ?MOD:to_val(123)).

to_tag_val_test() ->
	?assertEqual(<<"test">>, ?MOD:to_tag_val(test)),
	?assertEqual(<<"test">>, ?MOD:to_tag_val("test")),
	?assertEqual(<<"test">>, ?MOD:to_tag_val(<<"test">>)),
	?assertEqual(<<"true">>, ?MOD:to_tag_val(true)),
	?assertEqual(<<"false">>, ?MOD:to_tag_val(false)),
	?assertEqual(<<"te\\ st">>, ?MOD:to_tag_val(<<"te st">>)),
	?assertEqual(<<"te\\,st">>, ?MOD:to_tag_val(<<"te,st">>)),
	?assertEqual(<<"te\\\"st">>, ?MOD:to_tag_val(<<"te\"st">>)),
	?assertEqual(<<"t\\ e\\\"s\\,t\\\"">>, ?MOD:to_tag_val(<<"t e\"s,t\"">>)),
	?assertEqual(<<"123">>, ?MOD:to_tag_val(123)).


encode_time_test() ->
	?assertEqual(<<" 123">>, ?MOD:encode_time(123)),
	?assertEqual(<<"">>, ?MOD:encode_time(undefined)).

encode_2_proplist_point_test() ->
	?assertEqual(
	<<"test 1=1">>,
	?MOD:encode(test, [{1, 1}])
	).

encode_2_many_proplist_points_test() ->
	?assertEqual(
	<<"test 1=1 1\ntest 1=3 2\n">>,
	?MOD:encode("test", [ [{<<"1">>, 1}], [{<<"1">>, 3}] ], [], 1)
	).

encode_2_map_point_test() ->
	?assertEqual(
	<<"test 1=1">>,
	?MOD:encode(<<"test">>, #{1 => 1})
	).

encode_2_many_map_points_test() ->
	?assertEqual(
	<<"test 1=1 1\ntest 1=3 2\n">>,
	?MOD:encode(<<"test">>, [#{'1' => 1}, #{'1' => 3}], [], 1)
	).

encode_3_invalid_test() ->
	?assertEqual(
	{error, invalid_data},
	?MOD:encode(<<"test">>, 'bla-bla', [])
	).

encode_map_point_test() ->
	?assertEqual(
	<<"test,host=eu-west,ip=1.1.1.1 val=10,text=\"hello\" 123">>,
	?MOD:encode(
		#{
		measurement => <<"test">>,
		fields => [{"val", 10}, {text, <<"hello">>}],
		tags => [{host, "eu-west"}, {ip, '1.1.1.1'}],
		time => 123
		}
	)
	).

encode_proplist_point_test() ->
	?assertEqual(
	<<"test,host=eu-west,ip=1.1.1.1 val=10,text=\"hello\" 123">>,
	?MOD:encode(
		[
		{measurement, <<"test">>},
		{fields, [{"val", 10}, {text, <<"hello">>}]},
		{tags, #{host => "eu-west", ip => '1.1.1.1'}},
		{time, 123}
		]
	)
	).

encode_map_points_test() ->
	?assertEqual(
	<<"test val=10 1\ntest2 val=20 2\n">>,
	?MOD:encode(
		[
		#{
			measurement => <<"test">>,
			fields => #{"val" => 10},
			time => "1"
		},
		#{
			measurement => <<"test2">>,
			fields => #{"val" => 20},
			time => <<"2">>
		}
		]
	)
	).

encode_proplist_points_test() ->
	?assertEqual(
	<<"test val=10 1\ntest2 val=20 2\n">>,
	?MOD:encode(
		[
		[
			{measurement, <<"test">>},
			{fields, #{"val" => 10}},
			{time, 1}
		],
		[
			{measurement, <<"test2">>},
			{fields, #{"val" => 20}},
			{time, 2}
		]
		]
	)
	).

encode_map_without_measurement_test() ->
	?assertEqual(
	{error, invalid_data},
	?MOD:encode(
		#{
		fields => #{ val => 10}
		}
	) 
	).

encode_map_without_fields_test() ->
	?assertEqual(
	{error, invalid_data},
	?MOD:encode(
		#{
		measurement => test,
		tags => #{ val => 10}
		}
	) 
	).

encode_proplist_without_measurement_test() ->
	?assertEqual(
	{error, invalid_data},
	?MOD:encode(
		[
		{fields,  [{val, 10}] }
		]
	) 
	).

encode_proplist_without_fields_test() ->
	?assertEqual(
	{error, invalid_data},
	?MOD:encode(
		[
		{measurement, test},
		{tags, #{ val => 10}}
		]
	) 
	).

encode_at_least_one_invalid_test() ->
	?assertEqual(
	{error, invalid_data},
	?MOD:encode(
		[
		[
			{measurement, test},
			{fields, #{ val => 10}}
		],
		[
			{tags, #{ val => 10}}
		]
		]
	) 
	).

encode_3_with_proplist_tags_test() ->
	?assertEqual(
	<<"test,host=eu-west,ip=1.1.1.1 1=1">>,
	?MOD:encode(<<"test">>, #{1 => 1}, [{host, "eu-west"}, {ip, '1.1.1.1'}])
	).

encode_3_with_map_tags_test() ->
	?assertEqual(
	<<"test,host=eu-west,ip=1.1.1.1 1=1">>,
	?MOD:encode(<<"test">>, #{1 => 1}, #{host => 'eu-west', ip => <<"1.1.1.1">>})
	).

encode_3_with_binary_tags_test() ->
	?assertEqual(
	<<"test,host=eu-west,ip=1.1.1.1 1=1">>,
	?MOD:encode(<<"test">>, #{1 => 1}, <<",host=eu-west,ip=1.1.1.1">>)
	).

encode_3_many_points_with_tags_test() ->
	?assertEqual(
	<<"test,host=eu-west,num=111 memory=\"high\",cpu=20 100\ntest,host=eu-west,num=111 memory=\"low\",cpu=30 101\n">>,
	?MOD:encode(
		<<"test">>,
		[
		[{memory, "high"}, {"cpu", 20}],
		[{<<"memory">>, "low"}, {cpu, 30}]
		], 
		#{host => 'eu-west', num => 111},
		100
	)
	).

encode_4_test() ->
	?assertEqual(
	<<"test,host=eu-west,ip=1.1.1.1 1=1 123123">>,
	?MOD:encode(<<"test">>, #{1 => 1}, [{host, "eu-west"}, {ip, '1.1.1.1'}], 123123)
	).

encode_4_many_points_test() ->
	?assertEqual(
	<<"test,host=eu-west,ip=1.1.1.1 1=1,text=\"hello\" 123123">>,
	?MOD:encode(<<"test">>,
		[[{"1", 1}, {text, <<"hello">>}]],
		[{host, "eu-west"}, {ip, '1.1.1.1'}],
		123123
	)
	).

encode_escape_tags_test() ->
	?assertEqual(
	<<"test,host=eu\\ we\\=st,ip=1\\,1\\,1\\,1 1=1">>,
	?MOD:encode(<<"test">>,
		[[{"1", 1}]],
		[{host, "eu we=st"}, {ip, '1,1,1,1'}]
	)
	).

encode_escape_fields_test() ->
	?assertEqual(
	<<"test,host=eu-west 1=1,text=\"hello\\ \\\"man\\=\\=human\\,\\ yo!\\\"\"">>,
	?MOD:encode(<<"test">>,
		[[{"1", 1}, {text, <<"hello \"man==human, yo!\"">>}]],
		[{host, 'eu-west'}]
	)
	).
