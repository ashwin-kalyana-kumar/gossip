defmodule Random2D do
  defp get_empty_neighbors_map(pids, map) when pids == [] do
    map
  end

  defp get_empty_neighbors_map(pids, map) do
    pid = hd(pids)
    map = Map.put(map, pid, [])
    get_empty_neighbors_map(tl(pids), map)
  end

  defp assign_location(pids) when pids == [] do
    []
  end

  defp assign_location(pids) do
    x = :rand.uniform()
    y = :rand.uniform()
    [{hd(pids), x, y} | assign_location(tl(pids))]
  end

  defp check_neighbors(_, _, _, pids_with_loc, map) when pids_with_loc == [] do
    map
  end

  defp check_neighbors(pid, x, y, pids_with_loc, map) do
    # IO.inspect(map)
    [pid_with_loc | rest] = pids_with_loc
    {neighbor_pid, neighbor_x, neighbor_y} = pid_with_loc

    new_map =
      if(abs(x - neighbor_x) < 0.1 and abs(y - neighbor_y) < 0.1) do
        Map.update!(Map.update!(map, neighbor_pid, &[pid | &1]), pid, &[neighbor_pid | &1])
      else
        map
      end

    check_neighbors(pid, x, y, rest, new_map)
  end

  defp get_neighbour_pids(pids_with_loc, map) when pids_with_loc == [] do
    map
  end

  defp get_neighbour_pids(pids_with_loc, map) do
    [pid_with_loc | rest] = pids_with_loc
    {pid, x, y} = pid_with_loc
    map = check_neighbors(pid, x, y, rest, map)
    get_neighbour_pids(rest, map)
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
    neighbors_map = get_empty_neighbors_map(pids, neighbors_map)
    pids_with_loc = assign_location(pids)
    neighbors_map = get_neighbour_pids(pids_with_loc, neighbors_map)
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
    neighbors_map = get_empty_neighbors_map(pids, neighbors_map)
    pids_with_loc = assign_location(pids)
    # send_pids_to_ps_processes(pids_with_loc, n)
    #  IO.inspect(neighbors_map)
    neighbors_map = get_neighbour_pids(pids_with_loc, neighbors_map)
    #  IO.inspect(neighbors_map)
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
      # TODO start PushSumActor
      :push_sum -> start_push_sum(n)
    end
  end
end
