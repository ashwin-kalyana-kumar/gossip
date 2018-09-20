defmodule PushSumActor do
  use GenServer

  @doc """
  default startlink for the GenServer
  the status is of the following format
  {master_node, s, w, stop_val, old_ratio,[neighbor_nodes]}
  master_node -> the pid of the master node that started this gen_server process
  s, w -> s, w from the problem statement
  stop_val -> the value of the number of previous rounds which did not change
  more than 10^-10
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
  the method to send the gossip to a radom nieghbor of the current actor
  """
  def send_gossip(neighbors, message) do
    random_neighbor = neighbors |> Enum.random()
    GenServer.cast(random_neighbor, {:gossip, message})
  end

  @doc """
  the default handle_case for the GenServer. This is called whenever a new
  message is being sent to this process.
  """
  def handle_cast({:gossip, message}, state) do
    {master, s, w, stop_val, old_ratio, neighbors} = state
    {incoming_s, incoming_w} = message
    s = s + incoming_s
    w = w + incoming_w
    s = s / 2
    w = w / 2
    new_ratio = s / w
    new_diff = old_ratio - new_ratio
    min_val = Registry.lookup(:gossip_algo, :min_val)

    if(new_diff < min_val) do
      stop_val = stop_val + 1
    else
      stop_val = 0
    end

    if stop_val == 3 do
      send(master, :gossip_done)
    else
      send_gossip(neighbors, {s, w})
      state = {master, s, w, stop_val, new_ratio, neighbors}
    end

    {:noreply, state}
  end
end
