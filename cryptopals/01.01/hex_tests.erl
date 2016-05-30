-module(hex_tests).
-include_lib("eunit/include/eunit.hrl").

decode_test() -> ?assertEqual(
    <<1,35,69,103,137,171,205,239>>,
    hex:decode("0123456789abcdef")).
