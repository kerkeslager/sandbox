erl -make
erl -noshell -eval "eunit:test($1, [verbose])" -s init stop
