defmodule SieveFilter do
  def less_than?({a, _}, {b, _}) when a < b do true end
  def less_than?({a, b}, {a, c}) when b < c do true end
  def less_than?(_, _) do false end

  def get_next({counter, prime}) do {counter + prime, prime} end
end

defmodule Sieve do
  def run_item_through_sieve([{item, prime}|remaining_sieve], item) do
    {matched, unmatched} = run_item_through_sieve(remaining_sieve, item)
    {[{item, prime}|matched], unmatched}
  end
  def run_item_through_sieve(sieve, _) do {[], sieve} end

  def merge(xs, []) do xs end
  def merge([], ys) do ys end
  def merge(xs, ys) do
    [x|remaining_xs] = xs
    [y|remaining_ys] = ys

    if SieveFilter.less_than?(x, y) do
      [x|merge(remaining_xs, ys)]
    else
      [y|merge(xs, remaining_ys)]
    end
  end

  def get_next(sieve) do
    Enum.map(sieve, &(SieveFilter.get_next(&1)))
  end
end

defmodule Primes do
  def transform(sieve, previous_item) do
    item = previous_item + 1
    {matched, unmatched} = Sieve.run_item_through_sieve(sieve, item)

    case matched do
      [] -> {Sieve.merge([{item * 2, item}], sieve), item}
      _ -> transform(Sieve.merge(Sieve.get_next(matched), unmatched), item)
    end
  end

  def generate(initial_state, initial_item, get_next) do
    Stream.map(
      Stream.iterate(
        {initial_state, initial_item},
        fn({previous_state, previous_item}) -> get_next.(previous_state, previous_item) end
      ),
      fn({_, item}) -> item end
    )
  end

  def primes() do
    Stream.drop(generate([], 1, &(transform(&1, &2))), 1)
  end
end
