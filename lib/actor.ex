defmodule Actor do
  use GenServer

  @doc """
  default startlink for the GenServer
  the status is of the following format
  {master_node, number_of_messages_received, [neighbor_nodes]}
  master_node -> the pid of the master node that started this gen_server process
  number_of_messages_received -> the number of this actor has received this message
  [neighbor_nodes] -> the list of actors that are the neighbors of this actor
  """
  def start_link(status) do
    GenServer.start_link(__MODULE__, status)
  end

  @doc """
  the default init method for the GenServer
  """
  def init(status) do
    #    IO.inspect(status)
    {:ok, status}
  end

  def start_gossip(pid) do
    GenServer.cast(pid, {:gossip, :hello})
  end

  @doc """
  the method to send the gossip to a random nieghbor of the current actor
  """
  defp send_gossip(neighbors, message) do
    random_neighbor = neighbors |> Enum.random()
    GenServer.cast(random_neighbor, {:gossip, message})
  end

  def update_neighbors(pid, pids) do
    GenServer.call(pid, {:neighbors, pids})
  end

  def handle_call({:neighbors, pids}, _from, status) do
    {master, message_count, _} = status
    #  IO.inspect(master)
    {:reply, :ok, {master, message_count, pids}}
  end

  @doc """
  the default handle_case for the GenServer. This is called whenever a new
  message is being sent to this process.
  """
  def handle_cast({:gossip, message}, status) do
    {master, message_count, neighbors} = status
    message_count = message_count + 1
    #    IO.inspect(self())
    #    IO.puts(message_count)
    #    IO.inspect(master)

    if message_count === 10 do
      send(master, :gossip_done)
    else
      send_gossip(neighbors, message)
    end

    {:noreply, {master, message_count, neighbors}}
  end
end
