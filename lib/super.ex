defmodule Super do
  use Supervisor

  def start_link(children) do
    Supervisor.start_link(__MODULE__, children, strategy: :one_for_one, name: :supervisor)
  end

  def init(children, strategy) do
    Supervisor.init(children, strategy)
  end

  def init(children) do
    init(children, strategy: :one_for_one, name: :supervisor)
  end

  def terminate_random() do
    child_list = Supervisor.which_children(:supervisor)
    {id, _, _, _} = child_list |> Enum.random()
    Supervisor.terminate_child(:supervisor, id)
    id
  end

  def restart_child(id) do
    Supervisor.restart_child(:supervisor, id)
    nil
  end

  def get_random_child() do
    child_list = Supervisor.which_children(:supervisor)
    {_, pid, _, _} = child_list |> Enum.random()
    pid
  end

  def extract_pid(child_list) when child_list == [] do
    []
  end

  def extract_pid(child_list) do
    {_, pid, _, _} = hd(child_list)
    [pid | extract_pid(tl(child_list))]
  end

  def get_child_pids() do
    child_list = Supervisor.which_children(:supervisor)
    extract_pid(child_list)
  end

  def terminate_child(child_list) when child_list == [] do
    nil
  end

  def terminate_child(child_list) do
    {id, _, _, _} = hd(child_list)
    x = Supervisor.terminate_child(:supervisor, id)

    if(x === :ok) do
      Supervisor.delete_child(:supervisor, id)
    end

    terminate_child(tl(child_list))
  end

  def terminate_all() do
    child_list = Supervisor.which_children(:supervisor)
    terminate_child(child_list)
    Supervisor.stop(:supervisor, :normal, 2_000)
  end
end
