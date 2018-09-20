defmodule FullNetwork do
  defp start_gossip_proc(n) when n == 1 do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {Actor, :start_link, [{master, 0, []}]}
    }

    {:ok, child} = Super.start_child(child_spec)
    [child]
  end

  defp start_gossip_proc(n) do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {Actor, :start_link, [{master, 0, []}]}
    }

    #  IO.inspect(child_spec)
    {:ok, child} = Super.start_child(child_spec)
    [child | start_gossip_proc(n - 1)]
  end

  defp send_pids_to_processes(pids, n) when n == 1 do
    [pid | rest] = pids
    Actor.update_neighbors(pid, rest)
  end

  defp send_pids_to_processes(pids, n) do
    [pid | rest] = pids
    Actor.update_neighbors(pid, rest)
    rest = rest ++ [pid]
    send_pids_to_processes(rest, n - 1)
  end

  defp start_gossip(n) do
    Super.start_link(0)
    pids = start_gossip_proc(n)
    # IO.inspect(pids)
    send_pids_to_processes(pids, n)
    seed = Super.get_random_child()
    IO.puts("starting")
    Actor.start_gossip(seed)
  end

  ###############################################################################

  defp start_push_sum_proc(n) when n == 1 do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {PushSumActor, :start_link, [{master, n, 1, 0, n, []}]}
    }

    {:ok, child} = Super.start_child(child_spec)
    [child]
  end

  defp start_push_sum_proc(n) do
    [{_, master}] = Registry.lookup(:gossip_algo, :master)

    child_spec = %{
      id: n,
      start: {PushSumActor, :start_link, [{master, n, 1, 0, n, []}]}
    }

    #  IO.inspect(child_spec)
    {:ok, child} = Super.start_child(child_spec)
    [child | start_push_sum_proc(n - 1)]
  end

  defp send_pids_to_ps_processes(pids, n) when n == 1 do
    [pid | rest] = pids
    PushSumActor.update_neighbors(pid, rest)
  end

  defp send_pids_to_ps_processes(pids, n) do
    [pid | rest] = pids
    PushSumActor.update_neighbors(pid, rest)
    rest = rest ++ [pid]
    send_pids_to_ps_processes(rest, n - 1)
  end

  defp start_push_sum(n) do
    Super.start_link(0)
    pids = start_push_sum_proc(n)
    # IO.inspect(pids)
    send_pids_to_ps_processes(pids, n)
    seed = Super.get_random_child()
    IO.puts("starting")
    PushSumActor.start_gossip(seed)
  end

  def start(n, algorithm) do
    case algorithm do
      :gossip -> start_gossip(n)
      # TODO start PushSumActor
      :push_sum -> start_push_sum(n)
    end
  end
end
