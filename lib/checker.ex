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
end
