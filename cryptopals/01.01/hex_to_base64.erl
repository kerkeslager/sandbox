-module(hex_to_base64).
-export([hex_to_base64/1]).

hex_to_base64(Hex) -> base64:encode_to_string(hex:decode(Hex)).
