-module(stat).
-export([sum/1,mean/1,r_value/1,frequency/1,frequencies_to_scatterplot/2]).

sum([]) -> 0;
sum([Head|Tail]) -> Head + sum(Tail).

mean(List) -> sum(List) / length(List).

covariance(Points, MeanX, MeanY) ->
    sum(lists:map(fun({X,Y}) -> (X - MeanX) * (Y - MeanY) end, Points)).

standard_deviation(Samples, Mean) ->
    math:sqrt(sum(lists:map(
        fun(Item) -> math:pow(Item - Mean, 2) end,
        Samples))).

r_value(Points) ->
    Xs = lists:map(fun({X,_}) -> X end, Points),
    Ys = lists:map(fun({_,Y}) -> Y end, Points),
    MeanX = mean(Xs),
    MeanY = mean(Ys),
    Covariance = covariance(Points, MeanX, MeanY),
    StandardDeviationX = standard_deviation(Xs, MeanX),
    StandardDeviationY = standard_deviation(Ys, MeanY),
    Covariance / StandardDeviationX / StandardDeviationY.

frequency([],Result) -> Result;
frequency([Head|Tail],Result) ->
    frequency(Tail,dict:update_counter(Head,1,Result)).

frequency(Sample) -> frequency(Sample, dict:new()).

fetch_values(Dict) -> lists:map(
    fun({_,Value}) -> Value end,
    dict:to_list(Dict)).

frequencies_to_scatterplot(Fx,Fy) ->
    FxPoints = dict:map(fun(_,Value) -> {Value,0} end, Fx),
    FyPoints = dict:map(fun(_,Value) -> {0,Value} end, Fy),
    KeysToPoints = dict:merge(
        fun(_,{X,_},{_,Y}) -> {X,Y} end,
        FxPoints,
        FyPoints),
    fetch_values(KeysToPoints).
