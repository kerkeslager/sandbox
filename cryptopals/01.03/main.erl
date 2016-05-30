-module(main).
-export([run/0]).

% TODO Scoring function to identify which string is English

get_xored_possibilities() ->
    Base = hex:decode("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"),
    XorBytes = lists:seq(0,255),
    lists:map(fun(XorByte) -> hex:single_byte_xor(XorByte,Base) end, XorBytes).

get_comparison_text() ->
    {ok, Data} = file:read_file("1984.html"),
    Data.

print_all(List) -> lists:map(fun(S) -> io:format("~s~n",[S]) end, List).

run() -> print_all(get_xored_possibilities()).
