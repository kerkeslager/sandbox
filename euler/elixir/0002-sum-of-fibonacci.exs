defmodule SumOfEvenFibonacci do
  def fibonacci_stream() do
    items_with_prev = Stream.iterate({0,1}, fn({prev, curr}) -> {curr, prev + curr} end)
    Stream.map(items_with_prev, fn({_, curr}) -> curr end)
  end

  def fibonacci_to(limit) do
    Stream.take_while(fibonacci_stream(), fn(n) -> n < limit end)
  end

  def filter_even(stream) do
    Stream.filter(stream, fn(n) -> rem(n, 2) == 0 end)
  end

  def even_fibonacci_to(limit) do
    filter_even(fibonacci_to(limit))
  end
end

IO.inspect(Enum.sum(SumOfEvenFibonacci.even_fibonacci_to(4_000_000)))
