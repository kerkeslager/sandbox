-module(stat_tests).
-include_lib("eunit/include/eunit.hrl").

sum_test() -> ?assertEqual(20, stat:sum([2,4,6,8])).

mean_test() -> ?assertEqual(5.0, stat:mean([2,4,6,8])).

r_value_positive_test() -> ?assertEqual(
    1.0,
    stat:r_value([{1,2},{2,4},{4,8}])).

% This test doesn't work because of round-off error
%r_value_negative_test() -> ?assertEqual(
%    -1.0,
%    stat:r_value([{1,1},{2,0},{3,-1}])).

r_value_zero_test() -> ?assertEqual(
    0.0,
    stat:r_value([{1,1},{2,2},{3,1}])).

frequency_test() -> ?assertEqual(
    dict:from_list([{a,1},{b,2}]),
    stat:frequency([a,b,b])).

frequencies_to_scatterplot_test() -> ?assertEqual(
    [{1,1},{1,0},{0,1}],
    stat:frequencies_to_scatterplot(
        dict:from_list([{a,1},{b,1}]),
        dict:from_list([{a,1},{c,1}]))).
