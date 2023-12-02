-module(emsgpack_tests).

-include_lib("eunit/include/eunit.hrl").

%-define(TEST_MSGPACK, true).
-ifdef(TEST_MSGPACK).
msgpack() -> true.
-else.
msgpack() -> false.
-endif.

float_test() ->
    ?assertEqual(<<202,63,0,0,0>>, emsgpack:encode(1 / 2)),
    ?assertEqual(<<202,63,64,0,0>>, emsgpack:encode(3 / 4)),
    ?assertEqual(<<203,63,201,153,153,153,153,153,154>>, emsgpack:encode(1 / 5)),
    ?assertEqual(<<203,63,185,153,153,153,153,153,154>>, emsgpack:encode(1 / 10)),
    ?assertEqual(1 / 2, emsgpack:decode(<<202,63,0,0,0>>)),
    ?assertEqual(3 / 4, emsgpack:decode(<<202,63,64,0,0>>)),
    ?assertEqual(1 / 2, emsgpack:decode(<<203,63,224,0,0,0,0,0,0>>)),
    ?assertEqual(3 / 4, emsgpack:decode(<<203,63,232,0,0,0,0,0,0>>)),
    ?assertEqual(1 / 5, emsgpack:decode(<<203,63,201,153,153,153,153,153,154>>)),
    ?assertEqual(1 / 10, emsgpack:decode(<<203,63,185,153,153,153,153,153,154>>)),
    ok.

nil_test() ->
    ?assertEqual(<<192>>, emsgpack:encode([])),
    ?assertEqual(<<192>>, emsgpack:encode(null, [compat])),
    ?assertEqual(<<160>>, emsgpack:encode([], [compat])),
    ?assertEqual([], emsgpack:decode(<<192>>)),
    ?assertEqual(null, emsgpack:decode(<<192>>, [compat])),
    msgpack() andalso
    ?assertEqual({ok, null}, msgpack:unpack(<<192>>)),
    ok.

boolean_test() ->
    ?assertEqual(<<194>>, emsgpack:encode(false)),
    ?assertEqual(<<195>>, emsgpack:encode(true)),
    ?assertEqual(false, emsgpack:decode(<<194>>)),
    ?assertEqual(true, emsgpack:decode(<<195>>)),
    ok.

atom_test() ->
    ?assertEqual(<<217,4, "atom">>, emsgpack:encode(atom)),
    ?assertEqual(atom, emsgpack:decode(<<217,4, "atom">>)),
    ?assertEqual(atom, emsgpack:decode(<<217,4, "atom">>, [safe])),
    ?assertEqual(<<"atom">>, emsgpack:decode(<<217,4, "atom">>, [compat])),
    ?assertEqual(<<"ATOM">>, emsgpack:decode(<<217,4, "ATOM">>, [safe])),
    ?assertEqual(<<"atom", (binary:copy(<<"-long">>, 8))/binary>>,
                 emsgpack:decode(<<218,0,44,"atom", (binary:copy(<<"-long">>, 8))/binary>>, [safe])),
    ?assertEqual(<<"atom", (binary:copy(<<"-long">>, 8))/binary>>,
                 emsgpack:decode(<<218,0,44,"atom", (binary:copy(<<"-long">>, 8))/binary>>, [compat])),
    ?assertEqual(binary_to_atom(<<"atom", (binary:copy(<<"-long">>, 8))/binary>>, utf8),
                 emsgpack:decode(<<218,0,44,"atom", (binary:copy(<<"-long">>, 8))/binary>>)),
    ?assertEqual(<<218,0,44,"atom", (binary:copy(<<"-long">>, 8))/binary>>,
                 emsgpack:encode(binary_to_atom(<<"atom", (binary:copy(<<"-long">>, 8))/binary>>, utf8))),
    msgpack() andalso
    begin
    ?assertEqual({ok, "atom"}, msgpack:unpack(<<217,4, "atom">>)),
    ?assertEqual({ok, "atom" ++ string:copies("-long", 8)},
                 msgpack:unpack(<<218,0,44,"atom", (binary:copy(<<"-long">>, 8))/binary>>))
    end,
    ok.

string_test() ->
    ?assertEqual(<<166, "string">>, emsgpack:encode("string")),
    ?assertEqual("string", emsgpack:decode(<<166, "string">>)),
    ?assertEqual(<<217,46,"string", (binary:copy(<<"-long">>, 8))/binary>>,
                 emsgpack:encode("string" ++ string:copies("-long", 8))),
    ?assertEqual("string" ++ string:copies("-long", 8),
                 emsgpack:decode(<<217,46,"string", (binary:copy(<<"-long">>, 8))/binary>>)),
    msgpack() andalso
    begin
    ?assertEqual({ok, "string"}, msgpack:unpack(<<166, "string">>)),
    ?assertEqual({ok, "string" ++ string:copies("-long", 8)},
                 msgpack:unpack(<<217,46,"string", (binary:copy(<<"-long">>, 8))/binary>>))
    end,
    ok.

int_test() ->
    ?assertEqual(<<5>>, emsgpack:encode(5)),
    ?assertEqual(<<204, 155>>, emsgpack:encode(155)),
    ?assertEqual(<<205, 555:2/unit:8>>, emsgpack:encode(555)),
    ?assertEqual(<<206, 555555:4/unit:8>>, emsgpack:encode(555555)),
    ?assertEqual(<<207, 555555777777:8/unit:8>>, emsgpack:encode(555555777777)),
    ?assertEqual(111222333444555666777888999, emsgpack:decode(emsgpack:encode(111222333444555666777888999))),
    ?assertEqual(term_to_binary(111222333444555666777888999),
                 emsgpack:decode(emsgpack:encode(111222333444555666777888999), [compat])),
    ?assertEqual(<<-5>>, emsgpack:encode(-5)),
    ?assertEqual(<<208, -55>>, emsgpack:encode(-55)),
    ?assertEqual(<<209, -555:2/unit:8>>, emsgpack:encode(-555)),
    ?assertEqual(<<210, -555555:4/unit:8>>, emsgpack:encode(-555555)),
    ?assertEqual(<<211, -555555777777:8/unit:8>>, emsgpack:encode(-555555777777)),
    ?assertEqual(5, emsgpack:decode(<<5>>)),
    ?assertEqual(155, emsgpack:decode(<<204, 155>>)),
    ?assertEqual(555, emsgpack:decode(<<205, 555:2/unit:8>>)),
    ?assertEqual(555555, emsgpack:decode(<<206, 555555:4/unit:8>>)),
    ?assertEqual(555555777777, emsgpack:decode(<<207, 555555777777:8/unit:8>>)),
    ?assertEqual(-5, emsgpack:decode(<<-5>>)),
    ?assertEqual(-55, emsgpack:decode(<<208, -55>>)),
    ?assertEqual(-555, emsgpack:decode(<<209, -555:2/unit:8>>)),
    ?assertEqual(-555555, emsgpack:decode(<<210, -555555:4/unit:8>>)),
    ?assertEqual(-555555777777, emsgpack:decode(<<211, -555555777777:8/unit:8>>)),
    ?assertEqual(-111222333444555666777888999, emsgpack:decode(emsgpack:encode(-111222333444555666777888999))),
    ?assertEqual(term_to_binary(-111222333444555666777888999),
                 emsgpack:decode(emsgpack:encode(-111222333444555666777888999), [compat])),
    msgpack() andalso
    begin
    ?assertEqual({ok, 5}, msgpack:unpack(<<5>>)),
    ?assertEqual({ok, 155}, msgpack:unpack(<<204, 155>>)),
    ?assertEqual({ok, 555}, msgpack:unpack(<<205, 555:2/unit:8>>)),
    ?assertEqual({ok, 555555}, msgpack:unpack(<<206, 555555:4/unit:8>>)),
    ?assertEqual({ok, 555555777777}, msgpack:unpack(<<207, 555555777777:8/unit:8>>)),
    ?assertEqual({ok, -5}, msgpack:unpack(<<-5>>)),
    ?assertEqual({ok, -55}, msgpack:unpack(<<208, -55>>)),
    ?assertEqual({ok, -555}, msgpack:unpack(<<209, -555:2/unit:8>>)),
    ?assertEqual({ok, -555555}, msgpack:unpack(<<210, -555555:4/unit:8>>)),
    ?assertEqual({ok, -555555777777}, msgpack:unpack(<<211, -555555777777:8/unit:8>>)),
    ?assertEqual({ok, term_to_binary(111222333444555666777888999)},
                 msgpack:unpack(emsgpack:encode(111222333444555666777888999))),
    ?assertEqual({ok, term_to_binary(-111222333444555666777888999)},
                 msgpack:unpack(emsgpack:encode(-111222333444555666777888999)))
    end,
    ok.

binary_test() ->
    ?assertEqual(<<196, 6, "binary">>, emsgpack:encode(<<"binary">>)),
    ?assertEqual(<<"binary">>, emsgpack:decode(<<196, 6, "binary">>)),
    ?assertEqual(<<197, 506:2/unit:8, "binary", (binary:copy(<<"-long">>, 100))/binary>>,
                 emsgpack:encode(<<"binary", (binary:copy(<<"-long">>, 100))/binary>>)),
    ?assertEqual(<<"binary", (binary:copy(<<"-long">>, 100))/binary>>,
                 emsgpack:decode(<<197, 506:2/unit:8, "binary", (binary:copy(<<"-long">>, 100))/binary>>)),
    ?assertEqual(<<198, 500006:4/unit:8, "binary", (binary:copy(<<"-long">>, 100000))/binary>>,
                 emsgpack:encode(<<"binary", (binary:copy(<<"-long">>, 100000))/binary>>)),
    ?assertEqual(<<"binary", (binary:copy(<<"-long">>, 100000))/binary>>,
                 emsgpack:decode(<<198, 500006:4/unit:8, "binary", (binary:copy(<<"-long">>, 100000))/binary>>)),
    ?assertEqual(<<196, 7, "binary", 7:5, 0:3>>, emsgpack:encode(<<"binary", 7:5>>)),
    ?assertEqual(<<"binary", 7:5, 0:3>>, emsgpack:decode(<<196, 7, "binary", 7:5, 0:3>>)),
    msgpack() andalso
    begin
    ?assertEqual({ok, <<"binary">>}, msgpack:unpack(<<196, 6, "binary">>)),
    ?assertEqual({ok, <<"binary", (binary:copy(<<"-long">>, 100))/binary>>},
                 msgpack:unpack(<<197, 506:2/unit:8, "binary", (binary:copy(<<"-long">>, 100))/binary>>)),
    ?assertEqual({ok, <<"binary", (binary:copy(<<"-long">>, 100000))/binary>>},
                 msgpack:unpack(<<198, 500006:4/unit:8, "binary", (binary:copy(<<"-long">>, 100000))/binary>>))
    end,
    ok.

map_test() ->
    ?assertEqual(#{a => 1, b => #{c => 3}}, emsgpack:decode(emsgpack:encode(#{a => 1, b => #{c => 3}}))),
    ?assertEqual(#{<<$a>> => 1, <<$b>> => #{<<$c>> => 3}},
                 emsgpack:decode(emsgpack:encode(#{a => 1, b => #{c => 3}}), [compat])),
    msgpack() andalso
    ?assertEqual({ok, #{"a" => 1, "b" => #{"c" => 3}}},
                 msgpack:unpack(emsgpack:encode(#{a => 1, b => #{c => 3}}))),
    ok.

tuple_test() ->
    ?assertEqual({1, a, "string", {<<"binary">>, atom}},
                 emsgpack:decode(emsgpack:encode({1, a, "string", {<<"binary">>, atom}}))),
    ?assertEqual([1, <<$a>>, "string", [<<"binary">>, <<"atom">>]],
                 emsgpack:decode(emsgpack:encode({1, a, "string", {<<"binary">>, atom}}), [compat])),
    msgpack() andalso
    ?assertEqual({ok, [1, "a", "string", [<<"binary">>, "atom"]]},
                 msgpack:unpack(emsgpack:encode({1, a, "string", {<<"binary">>, atom}}))),
    ok.

list_test() ->
    ?assertEqual([1, a, "string", {<<"binary">>, atom}],
                 emsgpack:decode(emsgpack:encode([1, a, "string", {<<"binary">>, atom}]))),
    ?assertEqual([1, <<$a>>, "string", [<<"binary">>, <<"atom">>]],
                 emsgpack:decode(emsgpack:encode([1, a, "string", {<<"binary">>, atom}]), [compat])),
    ?assertEqual([1, a, "string"|atom],
                 emsgpack:decode(emsgpack:encode([1, a, "string"|atom]))),
    msgpack() andalso
    begin
    ?assertEqual({ok, [1, "a", "string", [<<"binary">>, "atom"]]},
                 msgpack:unpack(emsgpack:encode([1, a, "string", {<<"binary">>, atom}]))),
    ?assertEqual({ok, term_to_binary([1, a, "string"|atom])},
                 msgpack:unpack(emsgpack:encode([1, a, "string"|atom])))
    end,
    ok.

term_test() ->
    R = make_ref(),
    F = fun(E) -> (E * E) / 2 end,
    ?assertEqual(self(), emsgpack:decode(emsgpack:encode(self()))),
    ?assertEqual(R, emsgpack:decode(emsgpack:encode(R))),
    ?assertEqual(fun lists:map/2, emsgpack:decode(emsgpack:encode(fun lists:map/2))),
    ?assertEqual(F, emsgpack:decode(emsgpack:encode(F))),
    ?assertEqual(term_to_binary(self()), emsgpack:decode(emsgpack:encode(self()), [compat])),
    ?assertEqual(term_to_binary(R), emsgpack:decode(emsgpack:encode(R), [compat])),
    ?assertEqual(term_to_binary(fun lists:map/2), emsgpack:decode(emsgpack:encode(fun lists:map/2), [compat])),
    ?assertEqual(term_to_binary(F), emsgpack:decode(emsgpack:encode(F), [compat])),
    msgpack() andalso
    begin
    ?assertEqual({ok, term_to_binary(self())}, msgpack:unpack(emsgpack:encode(self()))),
    ?assertEqual({ok, term_to_binary(R)}, msgpack:unpack(emsgpack:encode(R))),
    ?assertEqual({ok, term_to_binary(fun lists:map/2)}, msgpack:unpack(emsgpack:encode(fun lists:map/2))),
    ?assertEqual({ok, term_to_binary(F)}, msgpack:unpack(emsgpack:encode(F)))
    end,
    ok.

timestamp_test() ->
    S = erlang:system_time(seconds),
    ?assertEqual(S, emsgpack:decode(<<214,-1, S:4/unit:8>>)),
    NS = erlang:system_time(nano_seconds),
    ?assertEqual(NS, emsgpack:decode(<<215,-1, (NS rem 1000000000):30, (NS div 1000000000):34>>)),
    ?assertEqual(NS, emsgpack:decode(<<199,12,-1, (NS rem 1000000000):4/unit:8, (NS div 1000000000):8/unit:8>>)),
    ok.
