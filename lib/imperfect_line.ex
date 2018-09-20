defmodule ImperfectLine do
  defp get_empty_neighbors_map(pids, map) when pids == [] do
    map
  end

  defp get_empty_neighbors_map(pids, map) do
    pid = hd(pids)
    map = Map.put(map, pid, [])
    get_empty_neighbors_map(tl(pids), map)
  end

  defp check_neighbors(neighbors) when neighbors == [] do
    []
  end

  defp check_neighbors(neighbors) do
    [neighbor | rest] = neighbors

    if neighbor == :doesnt_exist do
      check_neighbors(rest)
    else
      [neighbor | check_neighbors(rest)]
    end
  end

  defp get_neighbour_pids(n, max_n, id_map, map) when n == 0 do
    map
  end

  defp get_neighbour_pids(n, max_n, id_map, map) do
    pid = Map.get(id_map, n, :doesnt_exist)

    neighbors = [
      Map.get(id_map, n + 1, :doesnt_exist),
      Map.get(id_map, n - 1, :doesnt_exist),
      get_random_neighbor(n, max_n, id_map)
    ]

    neighbors = check_neighbors(neighbors)
    # map = Map.put(map, n, neighbors)
    map = Map.put(map, pid, neighbors)
    get_neighbour_pids(n - 1, max_n, id_map, map)
  end

  defp get_random_neighbor(n, max_n, id_map) do
    random_neighbor = :rand.uniform(max_n)

    if(random_neighbor == n - 1 or random_neighbor == n + 1) do
      Map.get(id_map, n, :doesnt_exist)
    else
      Map.get(id_map, random_neighbor, :doesnt_exist)
    end
  end

  ##############################################################################

  defp start_gossip_proc(n) when n == 1 do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {Actor, :start_link, [{master, 0, []}]}
    }

    [child_spec]
  end

  defp start_gossip_proc(n) do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {Actor, :start_link, [{master, 0, []}]}
    }

    [child_spec | start_gossip_proc(n - 1)]
  end

  defp send_pids_to_processes(pids, map, n) when n == 1 do
    [pid | rest] = pids
    neighbor_list = Map.get(map, pid)
    Actor.update_neighbors(pid, neighbor_list)
  end

  defp send_pids_to_processes(pids, map, n) do
    [pid | rest] = pids
    neighbor_list = Map.get(map, pid)
    Actor.update_neighbors(pid, neighbor_list)
    send_pids_to_processes(rest, map, n - 1)
  end

  defp start_gossip(n) do
    children = start_gossip_proc(n)
    Super.start_link(children)
    pids = Super.get_child_pids()
    neighbors_map = %{}
    #    neighbors_map = get_empty_neighbors_map(pids, neighbors_map)
    id_map = Super.get_child_info()
    neighbors_map = get_neighbour_pids(n, n, id_map, neighbors_map)
    send_pids_to_processes(pids, neighbors_map, n)
    seed = Super.get_random_child()
    IO.puts("starting")
    Actor.start_gossip(seed)
  end

  ############################################################################

  defp start_push_sum_proc(n) when n == 1 do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {PushSumActor, :start_link, [{n, master, n, 1, 0, n, []}]}
    }

    [child_spec]
  end

  defp start_push_sum_proc(n) do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {PushSumActor, :start_link, [{n, master, n, 1, 0, n, []}]}
    }

    [child_spec | start_push_sum_proc(n - 1)]
  end

  defp send_pids_to_ps_processes(pids, map, n) when n == 1 do
    [pid | rest] = pids
    neighbor_list = Map.get(map, pid)
    PushSumActor.update_neighbors(pid, neighbor_list)
  end

  defp send_pids_to_ps_processes(pids, map, n) do
    [pid | rest] = pids
    neighbor_list = Map.get(map, pid)
    PushSumActor.update_neighbors(pid, neighbor_list)
    send_pids_to_ps_processes(rest, map, n - 1)
  end

  defp start_push_sum(n) do
    children = start_push_sum_proc(n)
    Super.start_link(children)
    pids = Super.get_child_pids()
    neighbors_map = %{}
    # neighbors_map = get_empty_neighbors_map(pids, neighbors_map)
    # send_pids_to_ps_processes(pids_with_loc, n)
    #  IO.inspect(neighbors_map)
    id_map = Super.get_child_info()
    neighbors_map = get_neighbour_pids(n, n, id_map, neighbors_map)
    # IO.inspect(neighbors_map)
    send_pids_to_ps_processes(pids, neighbors_map, n)
    seed = Super.get_random_child()
    # TODO: check if the seed has any neighbors, else, choose another seed
    IO.puts("starting")
    IO.inspect(seed)
    PushSumActor.start_gossip(seed)
  end

  #############################################################################
  def start(n, algorithm) do
    case algorithm do
      :gossip -> start_gossip(n)
      :push_sum -> start_push_sum(n)
    end
  end
end
