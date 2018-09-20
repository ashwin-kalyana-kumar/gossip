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

  def listen_to_messages() do
    receive do
      :gossip_done ->
        IO.puts("done!!!")
        Super.terminate_all()

      {:restart_me, id} ->
        IO.puts("restarting child #{id}")
        Super.restart_child(id)
        listen_to_messages()
    after
      60_000 -> IO.puts("wtf bruh!")
    end
  end

  def main(n) do
    Registry.start_link(keys: :unique, name: :gossip_algo)
    Registry.register(:gossip_algo, :master, self())
    Registry.register(:gossip_algo, :min_val, 0.0000000001)
    Registry.register(:gossip_algo, :restart_threshold, 0.00000001)
    Registry.register(:gossip_algo, :restart_percent, 50)
    ImperfectLine.start(n, :gossip)
    listen_to_messages()
  end
end
