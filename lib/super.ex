defmodule Super do
  use DynamicSupervisor

  def start_link(args) do
    DynamicSupervisor.start_link(__MODULE__, args, name: :supervisor)
  end

  def init(_args) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(child_spec) do
    DynamicSupervisor.start_child(:supervisor, child_spec)
  end

  def terminate_random() do
    child_list = DynamicSupervisor.which_children(:supervisor)
    {id, pid, type, modules} = child_list |> Enum.random()
    DynamicSupervisor.terminate_child(:supervisor, id)
    id
  end

  def restart_child(id) do
    DynamicSupervisor.restart_child(:supervisor, id)
  end
end
