-module(hex).
-export([decode/1,encode/1,fixed_xor/2,single_byte_xor/2]).

data_in_pairs([]) -> [];
data_in_pairs([C1,C2|Tail]) -> [ [C1] ++ [C2] | data_in_pairs(Tail) ].
 
int_to_hex(N) when N < 256 -> [hex(N div 16), hex(N rem 16)].
 
hex(N) when (0  =< N) and (N < 10) -> $0 + N;
hex(N) when (10 =< N) and (N < 16) -> $a + (N-10).

decode(Data) -> [ list_to_integer(P,16) || P <- data_in_pairs(Data) ]. 
encode(L) -> string:join(lists:map(fun(X) -> int_to_hex(X) end, L), "").

fixed_xor(Data1,Data2) ->
    lists:zipwith(
        fun(P1,P2) -> P1 bxor P2 end,
        Data1,
        Data2).

single_byte_xor(Byte, Data) ->
    fixed_xor(lists:duplicate(length(Data),Byte),Data).
