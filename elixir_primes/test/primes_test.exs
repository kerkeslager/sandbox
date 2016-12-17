defmodule SieveFilterTest do
  use ExUnit.Case
  doctest SieveFilter

  test "less_than? compares the counter first" do
    assert SieveFilter.less_than?({10, 5}, {12, 3})
    assert not SieveFilter.less_than?({12, 3}, {10, 5})
  end

  test "less_than? compares the prime second" do
    assert SieveFilter.less_than?({10, 2}, {10, 5})
    assert not SieveFilter.less_than?({10, 5}, {10, 2})
  end

  test "get_next adds the prime to the counter" do
    assert SieveFilter.get_next({21, 7}) == {28, 7}
  end
end

defmodule SieveTest do
  use ExUnit.Case
  doctest SieveFilter

  test "run_item_through_sieve returns empty list and all filters for prime" do
    sieve = [{8, 2}, {9, 3}, {10, 5}]
    {matched, unmatched} = Sieve.run_item_through_sieve(sieve, 7)

    assert matched == []
    assert unmatched == sieve
  end

  test "run_item_through_sieve returns matched and unmatched filters for composite" do
    sieve = [{10, 2}, {10, 5}, {12, 3}, {14, 7}]
    {matched, unmatched} = Sieve.run_item_through_sieve(sieve, 10)

    assert matched == [{10, 2}, {10, 5}]
    assert unmatched == [{12, 3}, {14, 7}]
  end

  test "merge merges sieves" do
    sieve_a = [{12, 2}, {15, 5}]
    sieve_b = [{12, 3}, {14, 7}]
    merged = Sieve.merge(sieve_a, sieve_b)

    assert merged == [{12, 2}, {12, 3}, {14, 7}, {15, 5}]
  end

  test "get_next updates filters" do
    sieve = [{42, 2}, {42, 3}, {42, 7}]

    assert Sieve.get_next(sieve) == [{44, 2}, {45, 3}, {49, 7}]
  end
end

defmodule PrimesTest do
  use ExUnit.Case
  doctest Primes

  test "transform updates sieve and next item" do
    sieve = [{8, 2}, {9, 3}, {10, 5}, {14, 7}]
    item = 7

    {next_sieve, next_item} = Primes.transform(sieve, item)

    assert next_sieve == [{12, 2}, {12, 3}, {14, 7}, {15, 5}, {22, 11}]
    assert next_item == 11
  end

  test "generate iterates updating state and yielding items" do
    fibonacci = Primes.generate(0, 1, fn(prev, curr) -> {curr, prev + curr} end)

    assert Enum.take(fibonacci, 10) == [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
  end

  test "primes generates primes" do
    assert Enum.take(Primes.primes(), 10) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
  end

  test "primes is fast" do
    assert 1 == Enum.sum(Stream.take_while(Primes.primes(), &(&1 < 2_000_000)))
  end
end
