defmodule LargestPrimeFactor do
  def largest_prime_factor(start, n) do
    if start >= (n / 2) do
      n
    else
      if rem(n, start) == 0 do
        largest_prime_factor(start, div(n, start))
      else
        largest_prime_factor(start + 2, n)
      end
    end
  end
      
  def largest_prime_factor(n) do
    n_next = if rem(n, 2) == 0 do
      div(n, 2)
    else
      n
    end

    largest_prime_factor(3, n_next)
  end
end

IO.puts(LargestPrimeFactor.largest_prime_factor(600851475143))
