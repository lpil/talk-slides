# If you try this you'll find that the tests run multiple times in a row when
# you save one file.
# This is because there are actually many events occuring when a file is saved,
# and thus many messages sent to our Watcher.
#
# This gets even worse if we save another file while the tests run, as a
# backlog of messages continues to build up.
#
# To fix this we can empty our inbox of any messages immediately after running
# the tests.

defmodule Watcher do
  use GenServer

  # Public API

  def start_link
  do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  # Callbacks

  def init(_) do
    :ok = Application.ensure_started(:fs)
    :ok = :fs.subscribe()
    {:ok, []}
  end

  @source_file_pattern ~r/\.(exs|ex|erl|eex|xrl|yrl)\z/

  ### changes ###

  def handle_info({_, {:fs, :file_event}, {path, _}}, state) do
    if Regex.match?(@source_file_pattern, path) do
      Runner.run_tests()
      discard_messages() # Empty inbox after a test run
    end
    {:noreply, state}
  end

  def discard_messages do
    receive do
      _       -> discard_messages()
      after 0 -> :ok
    end
  end

  ###############
end

# Simply enter a receive loop that does nothing for all messages.
#
# This little `after 0` trick allows us to stop iteration if there are no
# messages left.
#
# And that's pretty much how this tool has worked for the last year or so.
