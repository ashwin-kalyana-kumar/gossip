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
    GenServer.start_link(status)
  end

  @doc """
  the default init method for the GenServer
  """
  def init(status) do
    {:ok, status}
  end

  @doc """
  the method to send the gossip to all the nieghbors of the current actor
  """
  def send_gossip(neighbors, _) when neighbors == [] do
    nil
  end

  def send_gossip(neighbors, message) do
    GenServer.cast(hd(neighbors), {:gossip, message})
    send_gossip(tl(neighbors, message))
  end

  @doc """
  the default handle_case for the GenServer. This is called whenever a new
  message is being sent to this process.
  """
  def handle_cast({:gossip, message}, status) do
    {master, message_count, neighbors} = status

    if message_count === 10 do
      send(master, :gossip_done)
    else
      send_gossip(neighbors)
    end

    {:noreply, state}
  end
end
