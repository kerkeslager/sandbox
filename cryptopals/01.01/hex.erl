-module(hex).
-export([decode/1]).

data_in_pairs([]) -> [];
data_in_pairs([C1,C2|Tail]) -> [ [C1] ++ [C2] | data_in_pairs(Tail) ].

decode(Data) -> << <<(list_to_integer(C,16)):8>> || C <- data_in_pairs(Data) >>.
