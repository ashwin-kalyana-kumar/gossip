defmodule Checker do
  def search(sqr, l, h) when l <= h do
    m = div(l + h, 2)
    m_sqr = m * m * m

    cond do
      m_sqr == sqr -> :success
      m_sqr < sqr -> search(sqr, m + 1, h)
      m_sqr > sqr -> search(sqr, l, m - 1)
    end
  end

  def search(_, l, _) do
    l
  end

  def get_factors(x, _) when x == 1 do
    [1]
  end

  def get_factors(x, n) do
    if(rem(n, x) == 0) do
      [x | get_factors(x - 1, n)]
    else
      get_factors(x - 1, n)
    end
  end

  def get_random_factor(n) do
    get_factors(trunc(:math.sqrt(n)) + 1, n) |> Enum.random()
  end
end
