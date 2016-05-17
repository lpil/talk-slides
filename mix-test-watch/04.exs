# Now we want to run the tests when we receive an event.
#
# Our Watcher has been changed so that it checks to see if the changed file has
# the extension of a BEAM source file, and then calls a function to run the
# tests.

defmodule Watcher do
  use GenServer

  # Public API

  def start_link
  do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  # Callbacks

  def init(_) do
    :ok = Application.ensure_started(:fs)
    :ok = :fs.subscribe
    {:ok, []}
  end

  ### changes ###

  @source_file_pattern ~r/\.(exs|ex|erl|eex|xrl|yrl)\z/

  def handle_info({_pid, {:fs, :file_event}, {path, _event}}, state) do
    if Regex.match?(@source_file_pattern, path) do
      Runner.run_tests()
    end
    {:noreply, state}
  end

  ###############
end

# Simple enough.
#
# So how does the run_tests/0 function work?
#
# Has anyone used Erlang Ports?
# Ports are a way of running commands in the shell. Anything that is printed by
# the command will be sent back to the process as a message.
#
# Here's our function.
# It creates a port that runs the command "mix test", and then enters a receive
# loop, printing the output from the tests running in the port.

defmodule Runner do
  def run_tests do
    {:spawn, "mix test"}
    |> Port.open(~w(stream binary exit_status use_stdio stderr_to_stdout)a)
    |> results_loop
    :ok
  end

  defp results_loop(port) do
    receive do
      {^port, {:data, data}} ->
        IO.write(data)
        results_loop(port)

      {^port, {:exit_status, status}} ->
        status
    end
  end
end

# And that's more or less what this project looked like at the begining.
# It does the job.
