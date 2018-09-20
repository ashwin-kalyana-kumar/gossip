defmodule Gossip do
  @moduledoc """
  Documentation for Gossip.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Gossip.hello()
      :world

  """
  def hello do
    :world
  end

  def main(n) do
    Registry.start_link(keys: :unique, name: :gossip_algo)
    Registry.register(:gossip_algo, :master, self())
    Registry.register(:gossip_algo, :min_val, 0.0000000001)
    FullNetwork.start(n, :push_sum)

    receive do
      :gossip_done -> IO.puts("done!!!")
    after
      10_000 -> IO.puts("wtf bruh!")
    end
  end
end
