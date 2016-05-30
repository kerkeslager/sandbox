-module(hex_tests).
-include_lib("eunit/include/eunit.hrl").

decode_test() -> ?assertEqual(
    [1,35,69,103,137,171,205,239],
    hex:decode("0123456789abcdef")).

encode_test() -> ?assertEqual(
    "0123456789abcdef",
    hex:encode([1,35,69,103,137,171,205,239])).

fixed_xor_test() -> ?assertEqual(
    "746865206b696420646f6e277420706c6179",
    hex:encode(hex:fixed_xor(
        hex:decode("1c0111001f010100061a024b53535009181c"),
        hex:decode("686974207468652062756c6c277320657965")))).
