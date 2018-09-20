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
    GenServer.start_link(__MODULE__, status)
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

  def start_gossip(pid) do
    GenServer.cast(pid, {:start_gossip})
  end

  def update_neighbors(pid, pids) do
    GenServer.call(pid, {:neighbors, pids})
  end

  def handle_call({:neighbors, pids}, _from, status) do
    {master, s, w, stop_val, old_ratio, _} = status
    #  IO.inspect(master)
    {:reply, :ok, {master, s, w, stop_val, old_ratio, pids}}
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

    new_diff =
      cond do
        old_ratio > new_ratio -> old_ratio - new_ratio
        new_ratio >= old_ratio -> new_ratio - old_ratio
      end

    [{_, min_val}] = Registry.lookup(:gossip_algo, :min_val)

    stop_val =
      if(new_diff < min_val) do
        stop_val + 1
      else
        0
      end

    if stop_val == 3 do
      send(master, :gossip_done)
    else
      send_gossip(neighbors, {s, w})
    end

    state = {master, s, w, stop_val, new_ratio, neighbors}
    {:noreply, state}
  end

  def handle_cast({:start_gossip}, state) do
    {master, s, w, stop_val, old_ratio, neighbors} = state
    s = s / 2
    w = w / 2
    new_ratio = s / w

    new_diff =
      cond do
        old_ratio > new_ratio -> old_ratio - new_ratio
        new_ratio >= old_ratio -> new_ratio - old_ratio
      end

    min_val = Registry.lookup(:gossip_algo, :min_val)

    stop_val =
      if(new_diff < min_val) do
        stop_val + 1
      else
        0
      end

    if stop_val == 3 do
      send(master, :gossip_done)
    else
      send_gossip(neighbors, {s, w})
    end

    state = {master, s, w, stop_val, new_ratio, neighbors}
    {:noreply, state}
  end
end
