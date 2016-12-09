defmodule SumOfMultiples do
  def is_multiple_of_3_or_5?(n) do
    (rem(n, 3) == 0) or (rem(n, 5) == 0)
  end

  def run(limit) do
    range = 0..(limit - 1)
    filtered_range = Enum.filter(range, &(is_multiple_of_3_or_5?(&1)))
    Enum.sum(filtered_range)
  end
end

IO.puts(SumOfMultiples.run(1000))
